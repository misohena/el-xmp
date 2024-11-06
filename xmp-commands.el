;;; xmp-commands.el --- Commands for editing XMP properties -*- lexical-binding: t; -*-

;; Copyright (C) 2024  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Files, Metadata, XMP, XML

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

;; This file provides a set of commands for manipulating the XMP
;; properties of any file.

;; The target file of those commands is inferred from the current
;; point, buffer, etc. by `xmp-file-name-at-point' function. By
;; default, filename inference from Dired, Image Dired, org-mode
;; links, thing-at-point, buffer-file-name, and read-file-name is
;; supported.

;; When the command modifies a property, it does not directly modify
;; the target file, but instead creates a sidecar file (.xmp) to
;; record the new property value. This method protects the contents of
;; the target file while facilitating the sharing of properties, e.g.,
;; via network storage.

;; Commands to change properties:

;; - `xmp-rate-file'
;; - `xmp-rate-file-1'
;; - `xmp-rate-file-2'
;; - `xmp-rate-file-3'
;; - `xmp-rate-file-4'
;; - `xmp-rate-file-5'
;; - `xmp-rate-file-0'
;; - `xmp-rate-file--1'
;; - `xmp-set-file-label'
;; - `xmp-set-file-subjects'
;; - `xmp-set-file-title'
;; - `xmp-set-file-description'
;; - `xmp-set-file-creators'
;; - `xmp-edit-file-properties'

;; Commands to display properties:
;; - `xmp-show-file-properties'
;; - `xmp-show-file-rating'
;; - `xmp-show-file-label'
;; - `xmp-show-file-subjects'
;; - `xmp-show-file-title'
;; - `xmp-show-file-description'
;; - `xmp-show-file-creators'

;; Functions to read and write properties taking sidecar files into account:
;; - `xmp-get-file-rating'
;; - `xmp-get-file-label'
;; - `xmp-get-file-subjects'
;; - `xmp-get-file-title-alist'
;; - `xmp-get-file-description-alist'
;; - `xmp-get-file-creators'

;;; Code:

(require 'xmp)

;;;; Prompt

(defun xmp-make-prompt-for-files (msg files-or-str current-value)
  (format msg
          ;; files
          (cond
           ((stringp files-or-str)
            files-or-str)
           ((listp files-or-str)
            (if (cdr files-or-str)
                (format (xmp-msg "%s files") (length files-or-str))
              (car files-or-str)))
           (t "-"))
          ;; current value
          (if current-value
              (concat " " (format (xmp-msg "(Current:%s)") current-value))
            "")))

;;;; File name at point

(defun xmp-file-name-at-point--fnapf (&rest _)
  "Return the file name obtained using the `file-name-at-point-functions'."
  ;; The following modes support `file-name-at-point-functions':
  ;; - dired-mode
  ;; - image-dired-thumbnail-mode
  ;; - ;;image-dired-image-mode ;; Bug
  (run-hook-with-args-until-success 'file-name-at-point-functions))

(defun xmp-file-name-at-point--image-file-buffer (&rest _)
  "If the current buffer's file name matches `image-file-name-regexp',
return it."
  (and (buffer-file-name)
       (string-match-p (image-file-name-regexp) (buffer-file-name))
       (buffer-file-name)))

(defun xmp-file-name-at-point--org-link (&rest _)
  "Return the path of the file link at the current point in org-mode."
  (and (derived-mode-p 'org-mode)
       (fboundp 'org-element-context)
       (fboundp 'org-element-type)
       (fboundp 'org-element-property)
       (when-let ((node (org-element-context)))
         (when (and (eq (org-element-type node) 'link)
                    (equal (org-element-property :type node) "file"))
           (when-let ((link-path (org-element-property :path node)))
             (let ((file (expand-file-name link-path)))
               (when (file-regular-p file)
                 file)))))))

(defun xmp-file-name-at-point--thing-at-point (&rest _)
  "Return the file name obtained using the `thing-at-point'."
  (thing-at-point 'existing-filename t))

(defun xmp-file-name-at-point--read-file-name (&rest _)
  "Return the file name obtained using the `read-file-name'."
  (read-file-name "File: " nil (buffer-file-name) t nil #'file-regular-p))

(autoload 'xmp-image-dired-get-marked-files "xmp-image-dired")

(defun xmp-file-name-at-point--image-dired-marked (multiple-p &rest _)
  "Return the list of files marked in image-dired-thumbnail-mode."
  (and (derived-mode-p 'image-dired-thumbnail-mode)
       multiple-p
       (xmp-image-dired-get-marked-files)))

(defcustom xmp-file-name-at-point-functions
  '(xmp-file-name-at-point--image-dired-marked
    xmp-file-name-at-point--fnapf
    xmp-file-name-at-point--image-file-buffer
    xmp-file-name-at-point--org-link
    xmp-file-name-at-point--thing-at-point
    xmp-file-name-at-point--read-file-name)
  "List of functions to try in sequence to get a file name at point."
  :type 'hook
  :group 'xmp)

(defun xmp-file-name-at-point ()
  "Return the target file name.

This function uses the `xmp-file-name-at-point-functions' hook to
determine the filename."
  (let ((result
         (run-hook-with-args-until-success 'xmp-file-name-at-point-functions
                                           ;; multiple-p
                                           nil)))
    (if (listp result)
        (car result)
      result)))

(defun xmp-file-name-list-at-point ()
  "Return the target file name list.

This function uses the `xmp-file-name-at-point-functions' hook to
determine the filename."
  (let ((result
         (run-hook-with-args-until-success 'xmp-file-name-at-point-functions
                                           ;; multiple-p
                                           t)))
    (if (stringp result)
        (list result)
      result)))


;;;; xmp:Rating
;; Type: Closed Choice of Real
;; https://developer.adobe.com/xmp/docs/XMPNamespaces/xmp/

(defun xmp-get-file-rating (file)
  "Return the rating of FILE."
  (xmp-pvalue-as-real
   (xmp-get-file-property file xmp-xmp:Rating)))

;;;###autoload
(defun xmp-show-file-rating (file)
  "Echo the rating of FILE."
  (interactive
   (list (xmp-file-name-at-point)))
  (message "%s" (xmp-get-file-rating file)))

(defconst xmp-rating-char-map
  '((?1 . 1) (?2 . 2) (?3 . 3) (?4 . 4) (?5 . 5) (?0 . 0)
    (?- . -1)))

(defun xmp-read-file-rating (files-or-str &optional current-rating)
  "Read the rating of FILE from user and return it.

CURRENT-RATING is displayed in prompt."
  (prog1
      (alist-get
       (read-char-choice
        (xmp-make-prompt-for-files
         (xmp-msg "Input rating (1-5, 0:Unrate, -:Reject) for `%s'%s: ")
         files-or-str
         current-rating)
        (mapcar #'car xmp-rating-char-map))
       xmp-rating-char-map)
    ;; Clear prompt
    (message nil)))
;; EXAMPLE: (xmp-read-file-rating "file" 3)
;; EXAMPLE: (xmp-read-file-rating "file")
;; EXAMPLE: (xmp-read-file-rating '("file1" "file2"))

;;;###autoload
(defun xmp-rate-file (files rating)
  "Set the rating of FILES to RATING.

FILES is a filename or a list of filenames.

RATING is a number that conforms to the specification for the xmp:Rating
property [XMP1 8.4 XMP namespace]. Valid numbers are -1 or a number
between 0 and 5. -1 means Rejected and 0 means Unrated. The numeric
value may be a real number, but an integer is recommended.  For
definition, see the description of xmp:Rating in URL
`https://developer.adobe.com/xmp/docs/XMPNamespaces/xmp/'.

If a prefix argument is specified, that number is used as RATING.

Where the property value is saved depends on the type of FILES and the
settings."
  (interactive
   (let* ((files (xmp-file-name-list-at-point))
          (current-rating (if (cdr files)
                              nil
                            (xmp-get-file-rating (car files))))
          (new-rating
           ;; From prefix argument
           (when current-prefix-arg
             (let ((n (prefix-numeric-value current-prefix-arg)))
               (when (and (integerp n) (<= -1 n 5))
                 (number-to-string n))))))
     (while (null new-rating)
       (setq new-rating (xmp-read-file-rating files
                                              (if (cdr files)
                                                  "-"
                                                (or current-rating
                                                    (xmp-msg "No property"))))))
     (when (equal new-rating current-rating)
       (error "No change"))
     (list files new-rating)))

  (unless (numberp rating)
    (signal 'wrong-type-argument (list 'numberp rating)))
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (xmp-set-file-property file xmp-xmp:Rating (xmp-pvalue-make-real rating))))

;;;###autoload
(defun xmp-rate-file-1 (files)
  "Set the rating of FILES to 1.
Equivalent to calling `xmp-rate-file' with a RATING argument of 1."
  (interactive (list (xmp-file-name-list-at-point)))
  (xmp-rate-file files 1))

;;;###autoload
(defun xmp-rate-file-2 (files)
  "Set the rating of FILE to 2.
Equivalent to calling `xmp-rate-file' with a RATING argument of 2."
  (interactive (list (xmp-file-name-list-at-point)))
  (xmp-rate-file files 2))

;;;###autoload
(defun xmp-rate-file-3 (files)
  "Set the rating of FILE to 3.
Equivalent to calling `xmp-rate-file' with a RATING argument of 3."
  (interactive (list (xmp-file-name-list-at-point)))
  (xmp-rate-file files 3))

;;;###autoload
(defun xmp-rate-file-4 (files)
  "Set the rating of FILE to 4.
Equivalent to calling `xmp-rate-file' with a RATING argument of 4."
  (interactive (list (xmp-file-name-list-at-point)))
  (xmp-rate-file files 4))

;;;###autoload
(defun xmp-rate-file-5 (files)
  "Set the rating of FILE to 5.
Equivalent to calling `xmp-rate-file' with a RATING argument of 5."
  (interactive (list (xmp-file-name-list-at-point)))
  (xmp-rate-file files 5))

;;;###autoload
(defun xmp-rate-file-0 (files)
  "Set the rating of FILE to 0.
Equivalent to calling `xmp-rate-file' with a RATING argument of 0."
  (interactive (list (xmp-file-name-list-at-point)))
  (xmp-rate-file files 0))

;;;###autoload
(defun xmp-rate-file--1 (files)
  "Set the rating of FILE to -1.
Equivalent to calling `xmp-rate-file' with a RATING argument of -1."
  (interactive (list (xmp-file-name-list-at-point)))
  (xmp-rate-file files -1))

(defun xmp-rating-match-p (rating condition)
  "Return non-nil if RATING matches CONDITION.

CONDITION is a string containing condition expressions that matches the
RATING. Multiple condition expressions can be specified, separated by
spaces, and a file will be marked if any of the conditions match (OR
condition). A condition expression is an integer from -1 to 5,
optionally preceded by a comparison operator (> >= = <= <)."
  (when (xmp-pvalue-maybe-p rating)
    (setq rating (xmp-pvalue-as-text rating)))
  (when (stringp rating)
    (setq rating (string-to-number rating)))
  (unless rating
    (setq rating 0))
  (unless (numberp rating)
    (signal 'wrong-type-argument (list 'numberp rating)))
  (let ((pos 0)
        (result nil))
    (while (and
            (null result)
            (progn
              (string-match
               "\\(?: *\\([<>]?=?\\)? *\\([0-5]\\|-1?\\)\\)\\|\\( *\\)\\'\\|"
               condition pos)
              (match-beginning 1)))
      (setq pos (match-end 0))
      (let* ((op (match-string 1 condition))
             (num-str (match-string 2 condition))
             (num (string-to-number (if (string= num-str "-") "-1" num-str))))
        (setq result (funcall (if (equal op "") '= (intern op)) rating num))))

    (when (and (null result)
               (null (match-beginning 3)))
      (error "Syntax error: %s" condition))

    result))
;; TEST: (xmp-rating-match-p 3 "2") => nil
;; TEST: (xmp-rating-match-p 3 "3") => t
;; TEST: (xmp-rating-match-p 3 ">-") => t
;; TEST: (xmp-rating-match-p 3 "<=2") => nil
;; TEST: (xmp-rating-match-p 3 "<=3") => t
;; TEST: (xmp-rating-match-p -1 "<=3") => t
;; TEST: (xmp-rating-match-p 3 "1 3 5") => t
;; TEST: (xmp-rating-match-p 3 "0 2 4  ") => nil
;; TEST: (xmp-rating-match-p 3 "<=1 >=4") => nil
;; TEST: (xmp-rating-match-p 5 "<=1 >=4  ") => t
;; TEST: (xmp-rating-match-p 2 "<3 error?") => t
;; TEST: (xmp-rating-match-p 5 "<3 error?") => error
;; TEST: (xmp-rating-match-p nil "=0") => t
;; TEST: (xmp-rating-match-p (xmp-pvalue-make-text "1") "=1") => t
;; TEST: (xmp-rating-match-p "1" "=1") => t

;;;; xmp:Label
;; Type: Text
;; https://developer.adobe.com/xmp/docs/XMPNamespaces/xmp/

(defconst xmp-label-strings ;; TODO: defcustom
  '(("Red" :color "red")
    ("Yellow" :color "yellow")
    ("Green" :color "green")
    ("Blue" :color "blue")
    ("Purple" :color "purple")
    ("Select")
    ("Second")
    ("Approved")
    ("Review")
    ("To Do")
    ("Final")
    ("Archived")))

(defun xmp-get-file-label (file)
  "Return the label of FILE."
  (xmp-pvalue-as-text
   (xmp-get-file-property file xmp-xmp:Label)))

;;;###autoload
(defun xmp-show-file-label (file)
  "Echo the label of FILE."
  (interactive
   (list (xmp-file-name-at-point)))
  (message "%s" (xmp-get-file-label file)))

(defun xmp-read-file-label (msg files-or-str current-label)
  (completing-read
   (xmp-make-prompt-for-files (or msg (xmp-msg "Change label of %s to%s: "))
                              files-or-str
                              current-label)
   (mapcar #'car xmp-label-strings)))
;; EXAMPLE: (xmp-read-file-label nil "foo.jpg" nil)
;; EXAMPLE: (xmp-read-file-label nil "foo.jpg" "Red")
;; EXAMPLE: (xmp-read-file-label nil "marked files" nil)

;;;###autoload
(defun xmp-set-file-label (files label)
  "Set the label of FILES to LABEL.

FILES is a filename or a list of filenames.

A LABEL is a word or short phrase used to classify a FILES.
Some image viewers will display a color if the color name is specified
in the label.
For definition, see the description of xmp:label in
URL `https://developer.adobe.com/xmp/docs/XMPNamespaces/xmp/'.

The variable `xmp-label-strings' is used as completion candidates.

Where the property value is saved depends on the type of FILES and the
settings."
  (interactive
   (let* ((files (xmp-file-name-list-at-point))
          (current-label (if (cdr files)
                             nil
                           (xmp-get-file-label (car files))))
          (new-label (xmp-read-file-label nil files current-label)))
     (when (equal new-label current-label)
       (error "No change"))
     (list files new-label)))
  (unless (stringp label)
    (signal 'wrong-type-argument (list 'stringp label)))
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (xmp-set-file-property file xmp-xmp:Label label)))

;;;; dc:subject (Tag)
;; Type: Unordered array of Text
;; https://developer.adobe.com/xmp/docs/XMPNamespaces/dc/
;; https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/subject/

(defun xmp-get-file-subjects (file)
  "Return the subjects of FILE."
  (xmp-pvalue-as-text-list
   (xmp-get-file-property file xmp-dc:subject)))

;;;###autoload
(defun xmp-show-file-subjects (file)
  "Echo the subjects of FILE."
  (interactive
   (list (xmp-file-name-at-point)))
  (message "%s" (mapconcat #'identity (xmp-get-file-subjects file) "; ")))

(defcustom xmp-read-subjects-candidates nil
  "A list of completion candidates for entering the subject."
  :type '(repeat string)
  :group 'xmp)

(defvar xmp-read-subjects--hist nil)

;; TODO: Move xmp-read-text-list* somewhere
(defvar xmp-read-text-list--hist nil)

(defun xmp-read-text-list (prompt current-text-list
                                  &optional candidates hist-var)
  (unless hist-var
    (setq hist-var 'xmp-read-text-list--hist))

  (set hist-var
       (seq-uniq (append
                  current-text-list
                  (symbol-value hist-var)
                  candidates)))

  (while (let ((input (completing-read
                       (format prompt (mapconcat #'identity current-text-list
                                                 "; "))
                       (symbol-value hist-var))))
           (unless (string-empty-p input)
             (if (member input current-text-list)
                 (setq current-text-list (remove input current-text-list))
               (setq current-text-list (append current-text-list (list input)))
               (set hist-var
                    (cons input (remove input (symbol-value hist-var)))))
             t)))
  current-text-list)
;; EXAMPLE: (xmp-read-text-list "Current subjects: %s\nSubject to toggle (empty to end): " nil)

(defun xmp-read-file-subjects (msg files-or-str current-subjects)
  (xmp-read-text-list
   (xmp-make-prompt-for-files
    (or msg
        (xmp-msg
         "Change subject of %s to: %%s\nSubject to toggle (empty to end): "))
    files-or-str nil)
   current-subjects
   xmp-read-subjects-candidates
   'xmp-read-subjects--hist))

;;;###autoload
(defun xmp-set-file-subjects (files subjects)
  "Set the subjects of FILES to SUBJECTS.

FILES is a filename or a list of filenames.

SUBJECTS is a list of subject strings. The order of strings in the list
has no meaning. A subject is a descriptive phrase, or keyword, that
describes the content of a FILES.
For definition, see the description of dc:subject in
URL `https://developer.adobe.com/xmp/docs/XMPNamespaces/dc/' and
URL `https://www.dublincore.org/specifications/dublin-core/dcmi-terms/terms/subject/'.

The variable `xmp-read-subjects-candidates' is used as completion candidates.

Where the property value is saved depends on the type of FILES and the
settings."
  (interactive
   (let* ((files (xmp-file-name-list-at-point))
          (current-subjects (if (cdr files)
                                nil
                              (xmp-get-file-subjects (car files))))
          (new-subjects (xmp-read-file-subjects nil files current-subjects)))
     (when (equal new-subjects current-subjects)
       (error "No change"))
     (list files new-subjects)))
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (xmp-set-file-property
     file
     xmp-dc:subject
     (xmp-pvalue-make-bag-from-text-list subjects))))

;;;###autoload
(defun xmp-add-file-subjects (files add-subjects)
  (interactive
   (let* ((files (xmp-file-name-list-at-point))
          (subjects (xmp-read-file-subjects
                     (xmp-msg "Add %%s to subjects of %s.\nSubject to toggle (empty to end): ")
                     files nil)))
     (unless subjects
       (error "No change"))
     (list files subjects)))
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (let* ((curr-subjects (xmp-get-file-subjects file))
           (new-subjects (seq-union curr-subjects add-subjects)))
      (xmp-set-file-property
       file
       xmp-dc:subject
       (xmp-pvalue-make-bag-from-text-list new-subjects)))))

;;;###autoload
(defun xmp-remove-file-subjects (files remove-subjects)
  (interactive
   (let* ((files (xmp-file-name-list-at-point))
          (subjects (xmp-read-file-subjects
                     (xmp-msg "Remove %%s from subjects of %s.\nSubject to toggle (empty to end): ")
                     files nil)))
     (unless subjects
       (error "No change"))
     (list files subjects)))
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (let* ((curr-subjects (xmp-get-file-subjects file))
           (new-subjects (seq-difference curr-subjects remove-subjects)))
      (xmp-set-file-property
       file
       xmp-dc:subject
       (xmp-pvalue-make-bag-from-text-list new-subjects)))))


;;;; dc:title
;; Type: Language Alternative
;; https://developer.adobe.com/xmp/docs/XMPNamespaces/dc/

(defun xmp-get-file-title-alist (file)
  "Return the title of FILE.

The results are returned in an alist with language code strings as keys."
  (xmp-pvalue-as-lang-alt-alist
   (xmp-get-file-property file xmp-dc:title)))

;;;###autoload
(defun xmp-show-file-title (file)
  "Echo the title of FILE."
  (interactive
   (list (xmp-file-name-at-point)))
  (message "%s" (xmp-lang-alt-alist-to-single-string
                 (xmp-get-file-title-alist file))))

;; TODO: Move xmp-read-lang-alt somewhere
(defun xmp-read-lang-alt (prompt current-alist)
  (list
   (cons "x-default"
         ;; TODO: If empty string entered?
         (read-string prompt
                      (cdar current-alist)))))

(defun xmp-read-file-title (msg files-or-str current-title-alist)
  (xmp-read-lang-alt
   (xmp-make-prompt-for-files (or msg (xmp-msg "Change title of %s to: "))
                              files-or-str nil)
   current-title-alist))
;; EXAMPLE: (xmp-read-file-title nil "test.jpg" '(("x-default" . "The Title")))

;;;###autoload
(defun xmp-set-file-title (files title)
  "Set the title of FILES to TITLE.

FILES is a filename or a list of filenames.

TITLE is a string or an alist whose keys are language code strings and
whose values are strings (`xmp-pvalue-from-lang-alt-alist' accepts
it). When specifying an alist, the first language code must be
\"x-default\"."
  (interactive
   (let* ((files (xmp-file-name-list-at-point))
          (current-title-alist (if (cdr files)
                                   nil
                                 (xmp-get-file-title-alist (car files))))
          (new-title-alist (xmp-read-file-title nil files current-title-alist)))
     (when (equal new-title-alist current-title-alist)
       (error "No change"))
     (list files new-title-alist)))
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (xmp-set-file-property
     file xmp-dc:title (xmp-pvalue-from-lang-alt-alist title))))

;;;; dc:description
;; Type: Language Alternative
;; https://developer.adobe.com/xmp/docs/XMPNamespaces/dc/

(defun xmp-get-file-description-alist (file)
  "Return the description of FILE.

The results are returned in an alist with language code strings as keys."
  (xmp-pvalue-as-lang-alt-alist
   (xmp-get-file-property file xmp-dc:description)))

;;;###autoload
(defun xmp-show-file-description (file)
  "Echo the description of FILE."
  (interactive
   (list (xmp-file-name-at-point)))
  (message "%s" (xmp-lang-alt-alist-to-single-string
                 (xmp-get-file-description-alist file))))

(defun xmp-read-file-description (msg files-or-str current-description-alist)
  (xmp-read-lang-alt
   (xmp-make-prompt-for-files (or msg (xmp-msg "Change description of %s to: "))
                              files-or-str nil)
   current-description-alist))
;; EXAMPLE: (xmp-read-file-description nil "test.jpg" nil)
;; EXAMPLE: (xmp-read-file-description nil "test.jpg" '(("x-default" . "The Description")))

;;;###autoload
(defun xmp-set-file-description (files description)
  "Set the description of FILES to DESCRTITLE.

FILES is a filename or a list of filenames.

DESCRIPTION is a string or an alist whose keys are language code strings
and whose values are strings (`xmp-pvalue-from-lang-alt-alist' accepts
it). When specifying an alist, the first language code must be
\"x-default\"."
  (interactive
   (let* ((files (xmp-file-name-list-at-point))
          (current-description-alist
           (if (cdr files)
               nil
             (xmp-get-file-description-alist (car files))))
          (new-description-alist (xmp-read-file-description
                                  nil files current-description-alist)))
     (when (equal new-description-alist current-description-alist)
       (error "No change"))
     (list files new-description-alist)))
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (xmp-set-file-property
     file xmp-dc:description (xmp-pvalue-from-lang-alt-alist description))))

;;;; dc:creator
;; Type: Ordered array of ProperName
;; https://developer.adobe.com/xmp/docs/XMPNamespaces/dc/

(defun xmp-get-file-creators (file)
  "Return a list of creators of FILE.
The elements of the list are strings."
  (xmp-pvalue-as-text-list
   (xmp-get-file-property file xmp-dc:creator)))

;;;###autoload
(defun xmp-show-file-creators (file)
  "Echo the creators of FILE."
  (interactive
   (list (xmp-file-name-at-point)))
  (message "%s" (mapconcat #'identity (xmp-get-file-creators file) "; ")))

(defcustom xmp-read-creators-candidates nil
  "A list of completion candidates for entering the creator name."
  :type '(repeat string)
  :group 'xmp)

(defvar xmp-read-creators--hist nil)

(defun xmp-read-file-creators (msg files-or-str current-creators)
  (xmp-read-text-list
   (xmp-make-prompt-for-files
    (or msg
        (xmp-msg
         "Change creators of %s to: %%s\nCreator to toggle (empty to end): "))
    files-or-str nil)
   current-creators
   xmp-read-creators-candidates
   'xmp-read-creators--hist))
;; (xmp-read-file-creators nil "marked files" '("AKIYAMA"))

;;;###autoload
(defun xmp-set-file-creators (files creators)
  "Set the creators of FILES to CREATORS.

FILES is a filename or a list of filenames.

CREATORS is a list of strings."
  (interactive
   (let* ((files (xmp-file-name-list-at-point))
          (current-creators (if (cdr files)
                                nil
                              (xmp-get-file-creators (car files))))
          (new-creators (xmp-read-file-creators nil files current-creators)))
     (when (equal new-creators current-creators)
       (error "No change"))
     (list files new-creators)))
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (xmp-set-file-property
     file
     xmp-dc:creator
     (xmp-pvalue-make-seq-from-text-list creators))))

;;;; Show properties

(defcustom xmp-show-file-properties-target
  '(("http://purl.org/dc/elements/1.1/" "title")
    ("http://purl.org/dc/elements/1.1/" "creator")
    ("http://purl.org/dc/elements/1.1/" "subject")
    ("http://purl.org/dc/elements/1.1/" "description")
    ("http://purl.org/dc/elements/1.1/" "date")
    ("http://ns.adobe.com/xap/1.0/" "CreateDate")
    ("http://ns.adobe.com/xap/1.0/" "Label")
    ("http://ns.adobe.com/xap/1.0/" "Rating"))
  "A list that specifies which properties to display in
command `xmp-show-file-properties'."
  :type '(choice
          (const :tag "All properties" nil)
          (repeat
           (list
            (string :tag "Namespace name (URI)")
            (string :tag "Property local name"))))
  :group 'xmp)

(defun xmp-show-file-properties-target-enames ()
  (if xmp-show-file-properties-target
      (cl-loop for (ns-name prop-local-name) in xmp-show-file-properties-target
               collect (xmp-xml-ename (xmp-xml-ns-name ns-name)
                                      prop-local-name))
    nil))

;;;###autoload
(defun xmp-show-file-properties (file &optional prop-ename-list)
  "Pop up a buffer showing the XMP properties of a FILE.

You can customize which properties are displayed by the variable
`xmp-show-file-properties-target'."
  (interactive
   (list
    (xmp-file-name-at-point)
    (if current-prefix-arg
        nil
      (xmp-show-file-properties-target-enames))))
  (let* ((ns-name-prefix-alist (xmp-xml-standard-ns-name-prefix-alist))
         (props (xmp-enumerate-file-properties file prop-ename-list
                                               ns-name-prefix-alist)))
    (if (null props)
        (message "No properties")
      (let ((buffer (get-buffer-create "*XMP Properties*")))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (princ (format (xmp-msg "File: %s") file) buffer)
            (princ "\n\n" buffer)
            (xmp-dump-named-pvalue-list buffer props ns-name-prefix-alist 0)
            (princ "\n" buffer)
            (xmp-dump-used-ns-name-prefix buffer props ns-name-prefix-alist))
          (goto-char (point-min))
          (view-mode))
        (pop-to-buffer buffer)))))

;;;###autoload
(defun xmp-show-file-properties-all (file)
  (interactive (list (xmp-file-name-at-point)))
  (xmp-show-file-properties file nil))

;;;; Editor

(autoload 'xmp-editor-buffer-modified-check "xmp-editor")
(autoload 'xmp-editor-open-files "xmp-editor")

;;;###autoload
(defun xmp-edit-file-properties (files &optional prop-spec-list)
  "Open a buffer for editing the properties in FILES.

The properties to edit are specified by PROP-SPEC-LIST. Generally, this
is nil, a list of property expanded names, or `all'. For the values that
can be specified here, see the description of
`xmp-editor-target-properties'. If it is nil, the value of
`xmp-editor-target-properties' is used.

If called as a command, FILES is inferred from the
`xmp-file-name-list-at-point' function.

If a prefix argument is specified, PROP-SPEC-LIST is `default-all'."
  (interactive
   (list (xmp-file-name-list-at-point)
         (when current-prefix-arg 'default-all)))
  (xmp-editor-buffer-modified-check)
  (xmp-editor-open-files files prop-spec-list))

;;;###autoload
(defun xmp-edit-file-properties-all (files)
  "Open a buffer for editing the properties in FILES.

This command calls `xmp-edit-file-properties' with `default-all' as the
PROP-SPEC-LIST argument."
  (interactive
   (list (xmp-file-name-list-at-point)))
  (xmp-edit-file-properties files 'default-all))

(provide 'xmp-commands)
;;; xmp-commands.el ends here
