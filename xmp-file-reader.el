;;; xmp-file-reader.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Files

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

;; A library for reading files in a similar way to common programming
;; languages.

;; Example:
;;   (with-temp-buffer
;;     (let ((reader (xmp-file-reader-open "example.bin")))
;;       ;; Read the offset in the file header and seek to it.
;;       (xmp-file-reader-seek reader (xmp-file-reader-u16be reader))
;;       ;; Read the byte sequence for the length recorded at current offset.
;;       (xmp-file-reader-read-bytes reader
;;                                   (xmp-file-reader-u16be reader))))

;; This allows partial reading of a file without loading the entire
;; file into a buffer.

;; The current buffer must be a temporary buffer that temporarily
;; holds the partial data read.

;; If there is insufficient data, it is loaded as needed, and data
;; that is no longer needed is deleted from the buffer.
;; The size of the temporary buffer is maintained based on the value
;; of the `xmp-file-reader-default-buffer-size' variable. The actual
;; size changes as needed.
;; By setting the `xmp-file-reader-keep-passed-region-p' variable to t,
;; you can prevent data that has already been read from being deleted
;; from the buffer. This consumes more memory, but is useful when
;; backtracking is required.

;;; Code:

;;;; Reader Object

(defun xmp-file-reader-create (file)
  (list 'xmp-file-reader file 0 nil nil))

(defmacro xmp-file-reader-file (reader) `(nth 1 ,reader))
(defmacro xmp-file-reader-buffer-beginning-offset (reader) `(nth 2 ,reader))
(defmacro xmp-file-reader-eof-loaded-p (reader) `(nth 3 ,reader))
(defmacro xmp-file-reader-file-size-cache (reader) `(nth 4 ,reader))

(defsubst xmp-file-reader-buffer-remainning () (- (point-max) (point)))
(defsubst xmp-file-reader-buffer-size () (- (point-max) (point-min)))
(defsubst xmp-file-reader-buffer-passed () (- (point) (point-min)))

(defsubst xmp-file-reader-buffer-end-offset (reader)
  (+ (xmp-file-reader-buffer-beginning-offset reader)
     (xmp-file-reader-buffer-size)))

(defsubst xmp-file-reader-current-offset (reader)
  (+ (xmp-file-reader-buffer-beginning-offset reader)
     (- (point) (point-min))))

;;;; Open

(defun xmp-file-reader-open (file)
  (erase-buffer)
  (set-buffer-multibyte nil)
  (xmp-file-reader-create file))

(defun xmp-file-reader-open-current-buffer ()
  (let ((reader (xmp-file-reader-create nil)))
    (setf (xmp-file-reader-eof-loaded-p reader) t)
    reader))

;;;; File Size

(defun xmp-file-reader-file-size (reader)
  (or (xmp-file-reader-file-size-cache reader)
      (setf (xmp-file-reader-file-size-cache reader)
            (or (file-attribute-size
                 (file-attributes
                  (or (xmp-file-reader-file reader)
                      (error "No file name assigned to file reader"))))
                (error "Failed to get file size")))))

;;;; Preloading

(defun xmp-file-reader-append (reader size)
  "Load and append SIZE bytes of subsequent file data to the end of the buffer.

Return the number of bytes actually appended."
  (if (and (> size 0)
           (not (xmp-file-reader-eof-loaded-p reader)))
      (save-excursion
        (let ((ins-point (point-max))
              (end-offset (xmp-file-reader-buffer-end-offset reader)))
          (goto-char ins-point)
          (xmp-file-reader-insert-file-contents (xmp-file-reader-file reader)
                                                end-offset
                                                (+ end-offset size))
          (let ((read-size (- (point-max) ins-point)))
            (when (< read-size size)
              ;; Reached EOF
              (setf (xmp-file-reader-eof-loaded-p reader) t))
            read-size)))
    0))

(defun xmp-file-reader-insert-file-contents (file beg end)
  (cond
   ((stringp file)
    (insert-file-contents-literally file nil beg end))
   ;; Emulate a file with a string.
   ((eq (car-safe file) 'xmp-file-reader-file-string)
    (let* ((str (cdr file))
           (size (length str)))
      (insert (substring str beg (min end size)))))))

(defun xmp-file-reader-file-string (string)
  "Create an alternative for a file whose content is a STRING."
  (cons 'xmp-file-reader-file-string string))

(defconst xmp-file-reader-default-buffer-size 4096)

(defvar xmp-file-reader-keep-passed-region-p nil)

(defun xmp-file-reader-ensure-bytes (reader size &optional noerror)
  "Ensure that there is at least SIZE bytes of data after point.

If there is not enough data, an error is signaled."
  ;; Reduce memory consumption
  (xmp-file-reader-discard-excess-passed-region reader)

  ;; If there is insufficient data, load it at the end of buffer
  (not ;; Return t if there is enough data
   (when (> size 0)
     (let ((shortage (- size (xmp-file-reader-buffer-remainning))))
       (when (> shortage 0)
         (xmp-file-reader-append
          reader
          (let ((m (% shortage xmp-file-reader-default-buffer-size)))
            (if (= m 0)
                shortage
              (+ (- shortage m) xmp-file-reader-default-buffer-size))))
         (if (< (xmp-file-reader-buffer-remainning) size)
             (if noerror
                 t
               (error "File is too short"))))))))

(defun xmp-file-reader-ensure-bytes-any (reader)
  "Ensure that there is subsequent data (if any) after point.

Ensures that there is at least 1 byte of data after point if the end of
the file has not been reached.

Does nothing if the end of the file has already been reached.

Return t if there is data available to read after point.  If the end of
the file has been reached and there is no more data to read, return nil."
  ;; Reduce memory consumption
  (xmp-file-reader-discard-excess-passed-region reader)

  (or
   ;; Still remaining
   (> (xmp-file-reader-buffer-remainning) 0)
   ;; Try to load
   (and
    (not (xmp-file-reader-eof-loaded-p reader))
    (> (xmp-file-reader-append reader xmp-file-reader-default-buffer-size) 0))))

;;;; Discard data

(defun xmp-file-reader-discard-excess-passed-region (reader)
  (when (and (not xmp-file-reader-keep-passed-region-p)
             (>= (xmp-file-reader-buffer-passed)
                 xmp-file-reader-default-buffer-size))
    (xmp-file-reader-discard-passed-region reader)))

(defun xmp-file-reader-discard-passed-region (reader)
  (let ((pt (point))
        (pt-min (point-min)))
    (delete-region pt-min pt)
    (cl-incf (xmp-file-reader-buffer-beginning-offset reader)
             (- pt pt-min))))

(defun xmp-file-reader-flush-buffer (reader new-offset)
  (erase-buffer)
  (setf (xmp-file-reader-buffer-beginning-offset reader) new-offset)
  (setf (xmp-file-reader-eof-loaded-p reader) nil))

;;;; Seek

(defun xmp-file-reader-seek (reader offset)
  (if (and (<= (xmp-file-reader-buffer-beginning-offset reader) offset)
           (if xmp-file-reader-keep-passed-region-p
               ;; Enable backtracking
               (<= offset (xmp-file-reader-buffer-end-offset reader))
             ;; Reduce memory consumption
             (< offset (xmp-file-reader-buffer-end-offset reader))))
      (goto-char (+ (point-min)
                    (- offset
                       (xmp-file-reader-buffer-beginning-offset reader))))
    (xmp-file-reader-flush-buffer reader offset)))

(defun xmp-file-reader-skip (reader size)
  (xmp-file-reader-seek reader
                        (+ (xmp-file-reader-current-offset reader) size)))

(defun xmp-file-reader-skip-chars (reader chars-spec &optional max-distance)
  (let ((count 0))
    (if max-distance
        (while (and (> max-distance 0)
                    (xmp-file-reader-ensure-bytes-any reader)
                    (let ((dist (skip-chars-forward chars-spec
                                                    (+ (point) max-distance))))
                      (cl-decf max-distance dist)
                      (cl-incf count dist)
                      (eobp))))
      (while (and (xmp-file-reader-ensure-bytes-any reader)
                  (progn
                    (cl-incf count (skip-chars-forward chars-spec))
                    (eobp)))))
    count))

(defun xmp-file-reader-skip-chars-if (reader pred &optional max-distance)
  (let ((count 0))
    (if max-distance
        (while (and (> max-distance 0)
                    (xmp-file-reader-ensure-bytes-any reader)
                    (let* ((beg (point))
                           (limit (min (+ beg max-distance) (point-max))))
                      (while (and (< (point) limit)
                                  (funcall pred (char-after)))
                        (forward-char))
                      (cl-decf max-distance (- (point) beg))
                      (cl-incf count (- (point) beg))
                      (eobp))))
      (while (and (xmp-file-reader-ensure-bytes-any reader)
                  (let ((beg (point)))
                    (while (and (not (eobp))
                                (funcall pred (char-after)))
                      (forward-char))
                    (cl-incf count (- (point) beg))
                    (eobp)))))
    count))

(defun xmp-file-reader-skip-to-u16zero (reader &optional max-bytes)
  (let ((ch1 nil))
    (xmp-file-reader-skip-chars-if reader
                                   (lambda (ch)
                                     (if ch1
                                         (if (and (= ch1 0) (= ch 0))
                                             nil ;; found
                                           (setq ch1 nil)
                                           t)
                                       (setq ch1 ch)
                                       t))
                                   max-bytes)))

;;;; Lookahead

(defun xmp-file-reader-scan-u8 (reader)
  (xmp-file-reader-ensure-bytes reader 1 'noerror)
  (char-after))

(defun xmp-file-reader-scan-bytes (reader size)
  (xmp-file-reader-ensure-bytes reader size)
  (buffer-substring-no-properties (point) (+ (point) size)))

(defun xmp-file-reader-scan-forward-bytes (reader distance size)
  (xmp-file-reader-ensure-bytes reader (+ distance size))
  (buffer-substring-no-properties (+ (point) distance)
                                  (+ (point) distance size)))

(defun xmp-file-reader-scan-chars-length (reader chars-spec)
  (let ((xmp-file-reader-keep-passed-region-p t)
        (begin (point))
        (end (progn
               (xmp-file-reader-skip-chars reader chars-spec)
               (point))))
    (goto-char begin)
    (- end begin)))

(defun xmp-file-reader-scan-chars (reader chars-spec)
  (buffer-substring-no-properties
   (point)
   (+ (point) (xmp-file-reader-scan-chars-length reader chars-spec))))

;;;; Read bytes

(defun xmp-file-reader-read-bytes (reader size)
  (xmp-file-reader-ensure-bytes reader size)
  (let* ((beg (point))
         (end (+ beg size)))
    (goto-char end)
    (buffer-substring beg end)))

(defun xmp-file-reader-read-chars (reader chars-spec &optional max-length)
  (let ((xmp-file-reader-keep-passed-region-p t)
        (begin (point)))
    (xmp-file-reader-skip-chars reader chars-spec max-length)
    (buffer-substring-no-properties begin (point))))

;;;; Read string

(defun xmp-file-reader-read-asciiz (reader)
  (prog1 (xmp-file-reader-read-chars reader "^\0")
    (unless (eobp)
      (forward-char))))

(defun xmp-file-reader-read-asciiz-field (reader field-length)
  (let ((value (xmp-file-reader-read-chars reader "^\0" field-length)))
    (xmp-file-reader-skip reader (- field-length (length value)))
    value))

(defun xmp-file-reader-read-utf16z-bytes (reader)
  (let ((xmp-file-reader-keep-passed-region-p t)
        (begin (point)))
    (xmp-file-reader-skip-to-u16zero reader)
    (prog1 (buffer-substring-no-properties begin (point))
      (when (and (zerop (char-after)) (zerop (char-after (1+ (point)))))
        (forward-char 2)))))

(defun xmp-file-reader-read-utf16z-bytes-field (reader field-bytes)
  (let ((xmp-file-reader-keep-passed-region-p t)
        (begin (point)))
    (xmp-file-reader-skip-to-u16zero reader)
    (prog1 (buffer-substring-no-properties begin (point))
      (xmp-file-reader-seek reader (+ begin field-bytes)))))

(defun xmp-file-reader-read-utf8z (reader)
  (decode-coding-string (xmp-file-reader-read-asciiz reader) 'utf-8))

(defun xmp-file-reader-read-utf-8-or-16-z (reader)
  (if (and (xmp-file-reader-ensure-bytes reader 2 'noerror)
           (let ((ch0 (char-after)) (ch1 (char-after (1+ (point)))))
             (or (and (= ch0 #xfe) (= ch1 #xff))
                 (and (= ch0 #xff) (= ch1 #xfe)))))
      (decode-coding-string (xmp-file-reader-read-utf16z-bytes reader) 'utf-16)
    (decode-coding-string (xmp-file-reader-read-asciiz reader) 'utf-8)))

;;;; Read integer

(defun xmp-file-reader-u8 (reader)
  (xmp-file-reader-ensure-bytes reader 1)
  (forward-char)
  (char-before))

(defun xmp-file-reader-u16be (reader)
  (xmp-file-reader-ensure-bytes reader 2)
  (let ((b1 (following-char)))
    (forward-char 2)
    (+ (ash b1 8) (preceding-char))))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\x12\x34\x56\x78")))) (cons (xmp-file-reader-u16be reader) (point)))) => (4660 . 3)

(defun xmp-file-reader-u24be (reader)
  (xmp-file-reader-ensure-bytes reader 3)
  (let ((pos (point)))
    (forward-char 3)
    (+ (ash (char-after pos) 16)
       (ash (char-after (+ pos 1)) 8)
       (char-after (+ pos 2)))))

(defun xmp-file-reader-u32be (reader)
  (xmp-file-reader-ensure-bytes reader 4)
  (let ((pos (point)))
    (forward-char 4)
    (+ (ash (char-after pos) 24)
       (ash (char-after (+ pos 1)) 16)
       (ash (char-after (+ pos 2)) 8)
       (char-after (+ pos 3)))))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\x12\x34\x56\x78")))) (cons (xmp-file-reader-u32be reader) (point)))) => (305419896 . 5)

(defun xmp-file-reader-u64be (reader)
  (xmp-file-reader-ensure-bytes reader 8)
  (let ((pos (point)))
    (forward-char 8)
    (+ (ash (char-after pos) 56)
       (ash (char-after (+ pos 1)) 48)
       (ash (char-after (+ pos 2)) 40)
       (ash (char-after (+ pos 3)) 32)
       (ash (char-after (+ pos 4)) 24)
       (ash (char-after (+ pos 5)) 16)
       (ash (char-after (+ pos 6)) 8)
       (char-after (+ pos 7)))))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\x12\x34\x56\x78\x02\x48\x13\x57")))) (cons (xmp-file-reader-u64be reader) (point)))) => (1311768464905999191 . 9)

(defun xmp-file-reader-uint-be (reader bytes)
  (xmp-file-reader-ensure-bytes reader bytes)
  (let ((result 0))
    (while (> bytes 0)
      (setq bytes (1- bytes)
            result (+ (ash result 8) (following-char)))
      (forward-char))
    result))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\x12\x34\x56\x78")))) (cons (xmp-file-reader-uint-be reader 4) (point)))) => (305419896 . 5)

(defun xmp-file-reader-u16le (reader)
  (xmp-file-reader-ensure-bytes reader 2)
  (let ((b1 (following-char)))
    (forward-char 2)
    (+ (ash (preceding-char) 8) b1)))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\x12\x34\x56\x78")))) (cons (xmp-file-reader-u16le reader) (point)))) => (13330 . 3)

(defun xmp-file-reader-u32le (reader)
  (xmp-file-reader-ensure-bytes reader 4)
  (let ((pos (point)))
    (forward-char 4)
    (+ (char-after pos)
       (ash (char-after (+ pos 1)) 8)
       (ash (char-after (+ pos 2)) 16)
       (ash (char-after (+ pos 3)) 24))))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\x12\x34\x56\x78")))) (cons (xmp-file-reader-u32le reader) (point)))) => (2018915346 . 5)

(defun xmp-file-reader-uint-le (reader bytes)
  (xmp-file-reader-ensure-bytes reader bytes)
  (let ((result 0)
        (pos 0))
    (while (> bytes 0)
      (setq bytes (1- bytes)
            result (+ result (ash (following-char) pos))
            pos (+ pos 8))
      (forward-char))
    result))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\x12\x34\x56\x78")))) (cons (xmp-file-reader-uint-le reader 4) (point)))) => (2018915346 . 5)

;; Signed integer

(defun xmp-file-reader-s8 (reader)
  (let ((n (xmp-file-reader-u8 reader)))
    (if (>= n #x80) (- n #x100) n)))

(defun xmp-file-reader-s16be (reader)
  (let ((n (xmp-file-reader-u16be reader)))
    (if (>= n #x8000) (- n #x10000) n)))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\xfe\xdc\xba\x98")))) (cons (xmp-file-reader-s16be reader) (point)))) => (-292 . 3)

(defun xmp-file-reader-s16le (reader)
  (let ((n (xmp-file-reader-u16le reader)))
    (if (>= n #x8000) (- n #x10000) n)))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\xdc\xfe\x98\xba")))) (cons (xmp-file-reader-s16le reader) (point)))) => (-292 . 3)

(defun xmp-file-reader-s32be (reader)
  (let ((n (xmp-file-reader-u32be reader)))
    (if (>= n #x80000000) (- n #x100000000) n)))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\xfe\xdc\xba\x98")))) (cons (xmp-file-reader-s32be reader) (point)))) => (-19088744 . 5)

(defun xmp-file-reader-s32le (reader)
  (let ((n (xmp-file-reader-u32le reader)))
    (if (>= n #x80000000) (- n #x100000000) n)))
;; TEST: (with-temp-buffer (let ((reader (xmp-file-reader-open (xmp-file-reader-file-string "\x98\xba\xdc\xfe")))) (cons (xmp-file-reader-s32le reader) (point)))) => (-19088744 . 5)


;; Dynamic endian

(defsubst xmp-file-reader-u16 (reader be)
  (if be (xmp-file-reader-u16be reader) (xmp-file-reader-u16le reader)))

(defsubst xmp-file-reader-u32 (reader be)
  (if be (xmp-file-reader-u32be reader) (xmp-file-reader-u32le reader)))

(defsubst xmp-file-reader-s16 (reader be)
  (if be (xmp-file-reader-s16be reader) (xmp-file-reader-s16le reader)))

(defsubst xmp-file-reader-s32 (reader be)
  (if be (xmp-file-reader-s32be reader) (xmp-file-reader-s32le reader)))

(provide 'xmp-file-reader)
;;; xmp-file-reader.el ends here
