;;; xmp-pdf.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Files,PDF,Metadata

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A library for extracting metadata from PDFs.
;; Does not support compression, encryption, or various other formats.
;; Use pdfinfo to obtain metadata more reliably.

;;; References:

;; [PDF2] ISO 32000-2:2020,
;;        Document management - Portable document format - Part 2: PDF 2.0
;;        https://pdfa.org/resource/iso-32000-2/

;;; Code:

(require 'xmp-file-reader)

;;;; Error

(defun xmp-pdf-error (reader msg &rest args)
  (error "%s (offset:%s file:%s)"
         (apply #'format msg args)
         (xmp-file-reader-current-offset reader)
         (xmp-file-reader-file reader)))

;;;; Character Classes

;; [PDF2] 7.2.3 Character set
(defconst xmp-pdf-chars-ws "\0\t\n\f\r ")
(defconst xmp-pdf-chars-delim "][()<>{}/%")
(defconst xmp-pdf-chars-regular "^][()<>{}/%\0\t\n\f\r ")
(defconst xmp-pdf-chars-not-regular "][()<>{}/%\0\t\n\f\r ")

(defconst xmp-pdf-re-ws "[\0\t\n\f\r ]")
(defconst xmp-pdf-re-delim "[][()<>{}/%]")
(defconst xmp-pdf-re-regular "[^][()<>{}/%\0\t\n\f\r ]")
(defconst xmp-pdf-re-non-regular "[][()<>{}/%\0\t\n\f\r ]")

(defconst xmp-pdf-re-eol "\\(?:\r\n\\|\r\\|\n\\)")

;;;; White-spaces and comments

;; [PDF2] 7.2.4 Comments
(defconst xmp-pdf-re-comment "\\(?:%[^\r\n]*\\)")
(defconst xmp-pdf-re-wsc
  (concat "\\(?:" xmp-pdf-re-comment "?" xmp-pdf-re-ws "+"
          "\\(?:" xmp-pdf-re-comment xmp-pdf-re-ws "+\\)*\\)"))
(defconst xmp-pdf-re-wsc-opt
  (concat xmp-pdf-re-wsc "?"))

(defun xmp-pdf-skip-white-spaces-and-comments (reader)
  (while (progn
           (xmp-file-reader-skip-chars reader xmp-pdf-chars-ws)
           ;; (xmp-file-reader-ensure-bytes-any reader) ;;Not required
           (when (eq (char-after) ?%)
             (forward-char)
             (xmp-file-reader-skip-chars reader "^\n\r")
             t))))

;;;; Objects

(defun xmp-pdf-read-object (reader &optional context)
  (xmp-pdf-skip-white-spaces-and-comments reader)

  (xmp-file-reader-ensure-bytes reader 2 'noerror)
  (pcase (char-after)
    ;; White-space
    ;;((or ?\0 ?\t ?\n ?\f ?\r ? )
    ;; Delimiter
    ;;(?% comment
    (?/ (xmp-pdf-read-name reader))
    (?\( (xmp-pdf-read-string-literal reader))
    (?\) (xmp-pdf-error reader "Unexpected `)'"))
    (?< (if (eq (char-after (1+ (point))) ?<)
            ;; <<
            (let ((dic (xmp-pdf-read-dictionary reader)))
              (or
               (xmp-pdf-read-stream-object-after-dictionary reader dic)
               dic))
          ;; <
          (xmp-pdf-read-hexadecimal-string reader)))
    (?> (if (eq (char-after (1+ (point))) ?>)
            ;; >>
            (if (eq context 'on-dictionary-key)
                (progn (forward-char 2) 'end-dictionary)
              (xmp-pdf-error reader "Unexpected `>>'"))
          ;; >
          (xmp-pdf-error reader "Unexpected `>'")))
    (?\[ (xmp-pdf-read-array reader))
    (?\] (if (eq context 'on-array)
             (progn (forward-char) 'end-array)
           (xmp-pdf-error reader "Unexpected `]'")))
    (?{ (xmp-pdf-error reader "Unexpected `{'"))
    (?} (xmp-pdf-error reader "Unexpected `}'"))
    ;; Regular characters
    (_
     (let ((token (xmp-file-reader-read-chars reader xmp-pdf-chars-regular)))
       (cond
        ;; [PDF2]7.3.2 Boolean objects
        ((string= token "true") t)
        ((string= token "false") nil)
        ;; [PDF2]7.3.9 Null object
        ((string= token "null") 'null)
        ;; [PDF2]7.3.3 Numeric objects
        ((string-match-p "\\`-?\\(?:[0-9]+\\.[0-9]*\\|\\.[0-9]+\\)\\'" token)
         (float (string-to-number token)))
        ((string-match-p "\\`-?[0-9]+\\'" token) ;; integer
         (or
          ;; Object Reference or Object Definition
          (xmp-pdf-read-indirect-object-after-integer reader token)
          ;; Integer
          (string-to-number token)))
        (t
         (xmp-pdf-error reader "Unexpected keyword `%s'" token)))))))

(defun xmp-pdf-read-name (reader)
  ;; [PDF2] 7.3.5 Name objects
  (unless (eq (xmp-file-reader-u8 reader) ?/)
    (xmp-pdf-error reader "Not a name"))
  (cons 'name
        ;; TODO: Signal an error if invalid notation? (#zz)
        (replace-regexp-in-string
         "#[0-9A-Fa-f][0-9A-Fa-f]"
         (lambda (num)
           (string (string-to-number (substring num 1) 16)))
         (xmp-file-reader-read-chars reader xmp-pdf-chars-regular)
         t)))

(defun xmp-pdf-read-string-literal (reader)
  ;; [PDF2] 7.3.4.2 Literal strings
  (unless (eq (xmp-file-reader-u8 reader) ?\()
    (xmp-pdf-error reader "Not a string literal"))
  (let ((result "")
        (num-paren 1))
    (while (> num-paren 0)
      (setq result
            (concat
             result
             ;; Normal characters
             (xmp-file-reader-read-chars reader "^()\r\\\\")
             ;; Special character
             (pcase (char-after)
               ('nil (xmp-pdf-error reader "String literal not closed")) ;;EOF
               (?\( (forward-char) (cl-incf num-paren) "(")
               (?\) (forward-char) (cl-decf num-paren)
                    (when (> num-paren 0) ")"))
               ;; Escape chars (\n \r \t \b \f \( \) \\ \eol \ddd)
               (?\\
                (forward-char)
                (xmp-file-reader-ensure-bytes reader 3 'noerror)
                (pcase (char-after)
                  (?n  (forward-char) (string ?\n)) ;; \n => "\n"
                  (?r  (forward-char) (string ?\r)) ;; \r => "\r"
                  (?t  (forward-char) (string ?\t)) ;; \t => "\t"
                  (?b  (forward-char) (string ?\b)) ;; \b => "\b"
                  (?f  (forward-char) (string ?\f)) ;; \f => "\f"
                  (?\( (forward-char) (string ?\()) ;; \( => "("
                  (?\) (forward-char) (string ?\))) ;; \) => ")"
                  (?\\ (forward-char) (string ?\\)) ;; \\ => "\"
                  (?\r (forward-char)
                       (when (eq (char-after) ?\n)
                         (forward-char))
                       "") ;; \eof => ""
                  (?\n (forward-char) "") ;; \eof => ""
                  (_ (if (looking-at "[0-7]\\{1,3\\}") ;; \ddd => "\ddd"
                         (progn
                           (goto-char (match-end 0))
                           (string (logand
                                    (string-to-number (match-string 0) 8)
                                    255)))
                       ;; Ignore \ if not matched
                       ))))
               ;; Unify EOL (\r or \r\n => \n)
               (?\r (forward-char)
                    (xmp-file-reader-ensure-bytes reader 1 'noerror)
                    (when (eq (char-after) ?\n)
                      (forward-char))
                    "\n")))))
    result))

(defun xmp-pdf-read-hexadecimal-string (reader)
  ;; [PDF2] 7.3.4.3 Hexadecimal strings
  (unless (eq (xmp-file-reader-u8 reader) ?\<)
    (xmp-pdf-error reader "Not a hexadecimal string"))
  (let* ((hex-len (xmp-file-reader-scan-chars-length reader "0-9a-fA-F"))
         (index 0)
         (result (make-string (/ (1+ hex-len) 2) 0)))
    (while (> hex-len 0)
      (let* ((b1 (prog1 (char-after) (forward-char)))
             (v1 (cond
                  ((<= ?0 b1 ?9) (- b1 ?0))
                  ((<= ?a b1 ?f) (+ (- b1 ?a) 10))
                  ((<= ?A b1 ?F) (+ (- b1 ?A) 10))))
             (b2 (if (= hex-len 1)
                     ?0
                   (prog1 (char-after) (forward-char))))
             (v2 (cond
                  ((<= ?0 b2 ?9) (- b2 ?0))
                  ((<= ?a b2 ?f) (+ (- b2 ?a) 10))
                  ((<= ?A b2 ?F) (+ (- b2 ?A) 10)))))
        (aset result index (+ (ash v1 8) v2))
        (setq index (1+ index)
              hex-len (- hex-len 2))))
    (if (eq (char-after) ?>)
        (forward-char)
      (xmp-pdf-error reader "Hexadecimal string not closed"))
    result))

(defun xmp-pdf-read-array (reader)
  ;; [PDF2] 7.3.6 Array objects
  (unless (eq (xmp-file-reader-u8 reader) ?\[)
    (xmp-pdf-error reader "Not a array"))
  (cons 'array
        (cl-loop for obj = (xmp-pdf-read-object reader 'on-array)
                 until (eq obj 'end-array)
                 collect obj)))

(defun xmp-pdf-read-dictionary (reader)
  ;; [PDF2] 7.3.7 Dictionary objects
  (unless (and (eq (xmp-file-reader-u8 reader) ?\<)
               (eq (xmp-file-reader-u8 reader) ?\<))
    (xmp-pdf-error reader "Not a dictionary"))
  (cons 'dictionary
        (cl-loop for key = (xmp-pdf-read-object reader 'on-dictionary-key)
                 until (eq key 'end-dictionary)
                 unless (eq (car-safe key) 'name)
                 do (xmp-pdf-error reader
                                   "Dictionary key is not a name (%s)" key)
                 for value = (xmp-pdf-read-object reader)
                 collect (cons (cdr key) value))))

(defun xmp-pdf-dic-get (key dictionary)
  (alist-get key dictionary nil nil #'equal))

(defconst xmp-pdf-re-stream-begin
  (concat "\\(stream\\(?:\r\n\\|\n\\)\\)")) ;; not by CR alone

(defconst xmp-pdf-re-stream-end
  (concat "\\(" xmp-pdf-re-eol "endstream" "\\)" xmp-pdf-re-non-regular))

(defun xmp-pdf-read-stream-object-after-dictionary (reader dic)
  ;; [PDF2] 7.3.8 Stream objects
  (xmp-pdf-skip-white-spaces-and-comments reader)

  (xmp-file-reader-ensure-bytes reader 8 'noerror) ;; stream\r\n
  (when (looking-at xmp-pdf-re-stream-begin)
    (let ((stream-size (xmp-pdf-dic-get "Length" dic)))
      (unless (integerp stream-size)
        (xmp-pdf-error reader "Length is not a integer"))
      (goto-char (match-end 1))
      (let ((stream-data-begin (xmp-file-reader-current-offset reader)))
        (xmp-file-reader-ensure-bytes reader stream-size)
        (xmp-file-reader-skip reader stream-size)
        (xmp-file-reader-ensure-bytes reader 12 'noerror) ;; \r\nendstream.
        (unless (looking-at xmp-pdf-re-stream-end)
          (xmp-pdf-error reader "Stream object not closed"))
        (goto-char (match-end 1))
        (cons 'stream
              (cons dic (cons stream-data-begin stream-size)))))))


(defconst xmp-pdf-re-indirect-object-header
  (concat "\\(\\([0-9]+\\)" xmp-pdf-re-wsc "\\(obj\\|R\\)\\)"
          xmp-pdf-re-non-regular))

(defun xmp-pdf-read-indirect-object-after-integer (reader integer-token)
  ;; [PDF2] 7.3.10 Indirect objects
  (xmp-pdf-skip-white-spaces-and-comments reader)
  (xmp-file-reader-ensure-bytes reader 128 'noerror) ;; TODO:
  (when (looking-at xmp-pdf-re-indirect-object-header)
    (goto-char (match-end 1))
    (let ((object-number (string-to-number integer-token))
          (generation-number (string-to-number (match-string-no-properties 2)))
          (keyword (match-string-no-properties 3)))
      (if (string= keyword "R")
          ;; Object Reference
          (cons 'objectref (cons object-number generation-number))
        ;; Object Definition
        ;; obj ~ endobj
        (let ((obj (xmp-pdf-read-object reader)))
          (xmp-pdf-skip-white-spaces-and-comments reader)
          (unless (equal (xmp-file-reader-read-chars reader
                                                     xmp-pdf-chars-regular)
                         "endobj")
            (xmp-pdf-error reader "Indirect object definition not closed"))
          ;; Strip object definition
          obj)))))


;;;; Stream Objects

(defun xmp-pdf-read-stream-object-content (reader object
                                                  xref-section
                                                  trailer-dic)
  ;; [PDF2] 7.3.8 Stream objects
  (when (eq (car-safe object) 'stream)
    (let ((dic (cadr object))
          (begin (caddr object))
          (size (cdddr object)))

      (when (xmp-pdf-deref reader (xmp-pdf-dic-get "Filter" dic)
                           xref-section trailer-dic)
        (xmp-pdf-error reader "Filter is not supported"))
      (when (xmp-pdf-deref reader (xmp-pdf-dic-get "F" dic)
                           xref-section trailer-dic)
        (xmp-pdf-error reader "Extra file is not supported"))

      (xmp-file-reader-seek reader begin)
      (list
       :bytes (xmp-file-reader-read-bytes reader size)
       :begin begin
       :size size
       :end (+ begin size)))))


;;;; Cross-reference table

;; [PDF2] 7.5.4 Cross-reference table
(defconst xmp-pdf-re-xref
  (concat "xref" xmp-pdf-re-wsc))
(defconst xmp-pdf-re-xref-subsection
  (concat "\\([0-9]+\\) +\\([0-9]+\\) *" xmp-pdf-re-eol))
(defconst xmp-pdf-re-xref-entry
  "\\([0-9]\\{10\\}\\) \\([0-9]\\{5\\}\\) \\(.\\)\\(?: \r\\| \n\\|\r\n\\)")
(defconst xmp-pdf-re-trailer
  (concat "trailer" xmp-pdf-re-wsc))

(defun xmp-pdf-read-xref-section (reader)
  ;; xref
  (xmp-file-reader-ensure-bytes reader 128 'noerror) ;; xref <wsc>
  (unless (looking-at xmp-pdf-re-xref)
    (xmp-pdf-error reader "No xref keyword"))
  (goto-char (match-end 0))
  ;; <subsections>...
  (cl-loop while (progn
                   (xmp-file-reader-ensure-bytes reader 64 'noerror)
                   (looking-at xmp-pdf-re-xref-subsection))
           do (goto-char (match-end 0))
           for first-objnum = (string-to-number (match-string 1))
           for num-entries = (string-to-number (match-string 2))
           collect (list first-objnum num-entries
                         (xmp-file-reader-current-offset reader))
           do (progn
                (xmp-file-reader-ensure-bytes reader (* num-entries 20))
                (xmp-file-reader-skip reader (* num-entries 20)))))

(defun xmp-pdf-xref-lookup-from-section (reader
                                         xref-section
                                         object-number generation-number)
  (when-let ((subsection
              (seq-find (lambda (subsection)
                          (let ((first-objnum (nth 0 subsection))
                                (num-entries (nth 1 subsection)))
                            (and (<= first-objnum object-number)
                                 (< object-number
                                    (+ first-objnum num-entries)))))
                        xref-section)))
    (xmp-file-reader-seek
     reader
     (+ (nth 2 subsection) (* (- object-number (nth 0 subsection)) 20)))

    (let ((entry-offset (string-to-number
                         (xmp-file-reader-read-bytes reader 10)))
          (_ (xmp-file-reader-u8 reader))
          (entry-gen (string-to-number
                      (xmp-file-reader-read-bytes reader 5)))
          (_ (xmp-file-reader-u8 reader))
          (entry-type (xmp-file-reader-u8 reader)))
      (when (and (eq entry-gen generation-number)
                 (eq entry-type ?n))
        entry-offset))))

(defun xmp-pdf-xref-lookup (reader xref-section trailer-dic
                                   object-number generation-number)
  (let (object-offset)
    (while (and
            (null (setq object-offset (xmp-pdf-xref-lookup-from-section
                                       reader xref-section
                                       object-number generation-number)))
            (let ((prev (xmp-pdf-dic-get "Prev" trailer-dic)))
              (when (integerp prev)
                ;;(message "Prev=%d" prev)
                (xmp-file-reader-seek reader prev)
                (setq xref-section (xmp-pdf-read-xref-section reader)
                      trailer-dic (xmp-pdf-read-trailer reader))
                t))))
    object-offset))

(defun xmp-pdf-deref (reader object xref-section trailer-dic)
  (if (eq (car-safe object) 'objectref)
      (let* ((object-number (cadr object))
             (generation-number (cddr object))
             (object-offset (xmp-pdf-xref-lookup reader
                                                 xref-section trailer-dic
                                                 object-number
                                                 generation-number)))
        (if object-offset
            (progn
              (xmp-file-reader-seek reader object-offset)
              (xmp-pdf-read-object reader))
          ;; Treat as null object if not found ([PDF2] 7.3.9 Null object)
          'null))
    object))

(defun xmp-pdf-read-xref-table-and-trailer (reader xref-offset)
  (progn
    ;; Read cross-reference table or cross-reference stream and trailer
    ;; [PDF2] 7.5.4 Cross-reference table
    (xmp-file-reader-seek reader xref-offset)
    (xmp-file-reader-ensure-bytes reader 128 'noerror)
    (cond
     ((looking-at-p xmp-pdf-re-xref)
      (cons (xmp-pdf-read-xref-section reader)
            (xmp-pdf-read-trailer reader)))
     ;; TODO: Support cross-reference stream
     (t
      (xmp-pdf-error reader "Unsupported cross-reference table format")))))

;;;; File Trailer

(defun xmp-pdf-read-trailer (reader)
  (xmp-file-reader-ensure-bytes reader (length "trailer\r\n"))
  (unless (looking-at xmp-pdf-re-trailer)
    (xmp-pdf-error reader "No trailer keyword"))
  (goto-char (match-end 0))
  (let ((trailer-dic (xmp-pdf-read-object reader)))
    (unless (eq (car-safe trailer-dic) 'dictionary)
      (xmp-pdf-error reader "No trailer dictionary"))
    trailer-dic))

;; [PDF2] 7.5.5 File trailer
(defconst xmp-pdf-re-startxref
  (concat xmp-pdf-re-eol "startxref" xmp-pdf-re-eol
          "\\([0-9]+\\)" xmp-pdf-re-eol
          "%%EOF" xmp-pdf-re-eol "?"))

(defun xmp-pdf-read-startxref (file)
  "Read the `startxref' section from the end of the PDF FILE and return the
offset of the first cross-reference section."
  (let* ((file-attr (or (file-attributes file)
                        (error "File not found (file:%s)" file)))
         (file-size (file-attribute-size file-attr)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil
                                      (max 0 (- file-size 256)) file-size)
      (goto-char (point-max))
      (unless (looking-back xmp-pdf-re-startxref (point-min))
        (error "No startxref (file:%s)" file))
      (string-to-number (match-string 1)))))

;;;; Metadata

(defun xmp-pdf-read-metadata (file)
  (let ((xref-offset (xmp-pdf-read-startxref file)))
    (with-temp-buffer
      (let* ((xmp-file-reader-keep-passed-region-p t) ;; Enable backtracking
             (reader (xmp-file-reader-open file))
             (xref-trailer (xmp-pdf-read-xref-table-and-trailer reader
                                                                xref-offset))
             (xref-section (car xref-trailer))
             (trailer-dic (cdr xref-trailer))
             (_
              (unless (memq (xmp-pdf-dic-get "Encrypt" trailer-dic) '(nil null)) ;;TODO: dereference indirect object to check for existence (treat it as null if it doesn't exist)
                (error "Encryption is not supported (file:%s)" file)))
             ;; Read Root (The catalog dictionary)
             (root (xmp-pdf-deref
                    reader
                    (xmp-pdf-dic-get "Root" trailer-dic)
                    xref-section
                    trailer-dic))
             (metadata (xmp-pdf-deref reader
                                      (xmp-pdf-dic-get "Metadata" root)
                                      xref-section
                                      trailer-dic)))


        ;; Read metadata
        (xmp-pdf-read-stream-object-content reader metadata
                                            xref-section trailer-dic)

        ;; ;; TODO: Retrieve /Info at the same time?
        ;; (xmp-pdf-deref reader
        ;;                (xmp-pdf-dic-get "Info" trailer-dic)
        ;;                xref-section
        ;;                trailer-dic)

        ))))

(provide 'xmp-pdf)
;;; xmp-pdf.el ends here
