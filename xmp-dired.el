;;; xmp-image-dired.el --- Dired extension for XMP  -*- lexical-binding: t; -*-

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

;; This file adds XMP-related features to Dired.

;; In addition to the commands in this file, the commands in
;; xmp-commands.el can also be used in Dired.

;;; Code:

(require 'dired)
(require 'xmp-commands)

;;;; Mark

(defun xmp-dired--mark-if (pred unflag-p &optional msg include-sidecar-file)
  (let ((dired-marker-char (if unflag-p ?\s dired-marker-char))
        (msg (or msg "matching file")))
    (dired-mark-if
     (and (not (looking-at-p dired-re-dot))
          (not (eolp)) ; empty line
          (let ((fn (dired-get-filename nil t)))
            (when (and fn
                       (or include-sidecar-file
                           (not (when-let ((ext (file-name-extension fn)))
                                  (string-equal-ignore-case ext "xmp"))))
                       ;; TODO: Mark directory ?
                       (file-regular-p fn))
              (let ((message-log-max nil))
                (message "%s %s" msg (file-name-nondirectory fn)))
              (funcall pred fn))))
     msg)))

;;;###autoload
(defun xmp-dired-mark-rating (condition unflag-p)
  "Mark all files whose rating matches CONDITION for use in later commands.
A prefix argument means to unmark them instead.

CONDITION is a string containing condition expressions that matches
ratings. Multiple condition expressions can be specified, separated by
spaces, and a file will be marked if any of the conditions match (OR
condition). A condition expression is an integer from -1 to 5,
optionally preceded by a comparison operator (> >= = <= <)."
  (interactive
   (list
    (read-string (xmp-msg "Mark rating (e.g. 1 3 >=5): "))
    current-prefix-arg)
   dired-mode)

  (xmp-dired--mark-if (lambda (file)
                        (xmp-rating-match-p  (or (xmp-get-file-rating file) "0")
                                             condition))
                      unflag-p))

;;;###autoload
(defun xmp-dired-mark-label (label unflag-p)
  "Mark all files whose label matches LABEL for use in later commands.
A prefix argument means to unmark them instead."
  (interactive
   (list
    (completing-read (xmp-msg "Mark label: ") (mapcar #'car xmp-label-strings))
    current-prefix-arg)
   dired-mode)
  (xmp-dired--mark-if (lambda (file) (equal (xmp-get-file-label file) label))
                      unflag-p))

;;;###autoload
(defun xmp-dired-mark-subjects (subjects unflag-p)
  "Mark all files whose subjects contain all of SUBJECTS for use in later
commands.
A prefix argument means to unmark them instead."
  (interactive
   (list
    (xmp-read-text-list
     (xmp-msg "Mark subjects (AND): %s\nSubject to toggle (empty to end): ")
     nil
     xmp-read-subjects-candidates
     'xmp-read-subjects--hist)
    current-prefix-arg)
   dired-mode)
  (xmp-dired--mark-if
   (lambda (file)
     (let ((file-subjects (xmp-get-file-subjects file)))
       (seq-every-p (lambda (s) (member s file-subjects)) subjects)))
   unflag-p))

;;;###autoload
(defun xmp-dired-mark-title (title-regexp unflag-p)
  "Mark all files whose title matches TITLE-REGEXP for use in later commands.
A prefix argument means to unmark them instead."
  (interactive
   (list
    (read-string (xmp-msg "Mark title (Regexp): "))
    current-prefix-arg)
   dired-mode)
  (xmp-dired--mark-if (lambda (file)
                        ;; TODO: Create xmp-lang-alt-alist-match-p ?
                        (seq-find (lambda (item)
                                    (string-match-p title-regexp (cdr item)))
                                  (xmp-get-file-title-alist file)))
                      unflag-p))

;;;###autoload
(defun xmp-dired-mark-description (description-regexp unflag-p)
  "Mark all files whose description matches DESCRIPTION-REGEXP for use in
later commands.
A prefix argument means to unmark them instead."
  (interactive
   (list
    (read-string (xmp-msg "Mark description (Regexp): "))
    current-prefix-arg)
   dired-mode)
  (xmp-dired--mark-if (lambda (file)
                        ;; TODO: Create xmp-lang-alt-alist-match-p ?
                        (seq-find (lambda (item)
                                    (string-match-p description-regexp
                                                    (cdr item)))
                                  (xmp-get-file-description-alist file)))
                      unflag-p))

;;;###autoload
(defun xmp-dired-mark-creator (creator-regexp unflag-p)
  "Mark all files whose creators matche CREATOR-REGEXP for use in
later commands.
A prefix argument means to unmark them instead."
  (interactive
   (list (read-string (xmp-msg "Mark creator (Regexp): "))
         current-prefix-arg)
   dired-mode)
  (xmp-dired--mark-if (lambda (file)
                        (seq-find (lambda (item)
                                    (string-match-p creator-regexp item))
                                  (xmp-get-file-creators file)))
                      unflag-p))

;;;;; Mark Sidecar files

;;;###autoload
(defun xmp-dired-mark-stray-sidecar-files (unflag-p)
  "Mark all stray sidecar files.
A prefix argument means to unmark them instead."
  (interactive "P" dired-mode)
  (let (dir-stray-sidecar-files-alist) ;; Cache
    (xmp-dired--mark-if
     (lambda (file)
       (when (xmp-sidecar-file-p file)
         (let* ((file (expand-file-name file))
                (dir (file-name-directory file))
                (stray-sidecar-files
                 (cdr
                  (or
                   (assoc dir dir-stray-sidecar-files-alist #'string=)
                   (car (push (cons dir (xmp-stray-sidecar-files-in-dir dir))
                              dir-stray-sidecar-files-alist))))))
           (not (null (member file stray-sidecar-files))))))
     unflag-p
     nil
     ;; Include .xmp
     t)))

;;;; Change properties

;; Although the commands in xmp-commands.el can handle this to some
;; extent, we will create dedicated functions to handle prefix ARG.

(defun xmp-dired-make-prompt (msg arg files current-value)
  (format msg
          (dired-mark-prompt arg files)
          (if current-value
              (concat
               " "
               (format (xmp-msg "(Current:%s)") current-value))
            "")))

;;;###autoload
(defun xmp-dired-do-rate (&optional arg)
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (rating (xmp-read-file-rating (dired-mark-prompt arg files)
                                       ;; current value
                                       (unless (cdr files)
                                         (xmp-get-file-rating (car files))))))
    (xmp-rate-file files rating)
    (dired-post-do-command)))

;;;###autoload
(defun xmp-dired-do-set-label (&optional arg)
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (label (xmp-read-file-label
                 nil (dired-mark-prompt arg files)
                 ;; current value
                 (unless (cdr files)
                   (xmp-get-file-label (car files))))))
    (xmp-set-file-label files label)
    (dired-post-do-command)))

;;;###autoload
(defun xmp-dired-do-set-subjects (&optional arg)
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (subjects (xmp-read-file-subjects
                    nil (dired-mark-prompt arg files)
                    ;; current value
                    (unless (cdr files)
                      (xmp-get-file-subjects (car files))))))
    (xmp-set-file-subjects files subjects)
    (dired-post-do-command)))

;;;###autoload
(defun xmp-dired-do-add-subjects (&optional arg)
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (subjects (xmp-read-file-subjects
                    (xmp-msg "Add %%s to subjects of %s.\nSubject to toggle (empty to end): ")
                    (dired-mark-prompt arg files)
                    nil)))
    (xmp-add-file-subjects files subjects)
    (dired-post-do-command)))

;;;###autoload
(defun xmp-dired-do-remove-subjects (&optional arg)
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (subjects (xmp-read-file-subjects
                    (xmp-msg "Remove %%s from subjects of %s.\nSubject to toggle (empty to end): ")
                    (dired-mark-prompt arg files)
                    nil)))
    (xmp-remove-file-subjects files subjects)
    (dired-post-do-command)))

;;;###autoload
(defun xmp-dired-do-set-title (&optional arg)
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (title (xmp-read-file-title
                 nil
                 (dired-mark-prompt arg files)
                 ;; current value
                 (unless (cdr files)
                   (xmp-get-file-title-alist (car files))))))
    (xmp-set-file-title files title)
    (dired-post-do-command)))

;;;###autoload
(defun xmp-dired-do-set-description (&optional arg)
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (description (xmp-read-file-description
                       nil (dired-mark-prompt arg files)
                       ;; current value
                       (unless (cdr files)
                         (xmp-get-file-description-alist (car files))))))
    (xmp-set-file-description files description)
    (dired-post-do-command)))

;;;###autoload
(defun xmp-dired-do-set-creators (&optional arg)
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (creators (xmp-read-file-creators
                    nil (dired-mark-prompt arg files)
                    ;; current value
                    (unless (cdr files)
                      (xmp-get-file-creators (car files))))))
    (xmp-set-file-creators files creators)
    (dired-post-do-command)))

;;;; Edit properties

;;;###autoload
(defun xmp-dired-do-edit-properties (&optional arg)
  "Open a buffer for editing the file's XMP properties.

If the prefix argument ARG is 0 or -, all properties present in the
current files are displayed. In other cases, the target file is selected
in the same way as the general dired command."
  (interactive "P" dired-mode)
  (let ((prop-spec-list (when (or (eq arg 0) (eq arg '-)) 'default-all)))
    (when prop-spec-list
      (setq arg nil))

    (xmp-edit-file-properties (dired-get-marked-files t arg nil nil t)
                              prop-spec-list)))

;;;###autoload
(defun xmp-dired-do-edit-properties-all (&optional arg)
  "Open a buffer for editing the file's XMP properties.

This command, unlike `xmp-dired-do-edit-properties', always shows all
properties present in the file.

The argument ARG is used to select the target file, just like in other
Dired commands."
  (interactive "P" dired-mode)
  (xmp-edit-file-properties (dired-get-marked-files t arg nil nil t)
                            'default-all))

;;;; Listing Modifications (Filter / Sort / Add Columns)

;; For property cache

(defconst xmp-dired-lsmod-required-properties-source
  (list #'xmp-dired-filter-required-properties
        #'xmp-dired-sort-required-properties
        'xmp-dired-add-column-properties))

(defvar xmp-dired-lsmod-required-properties nil)

(defvar xmp-dired-lsmod-files-properties-hash nil)

;; For filtering

(defvar-local xmp-dired-filter-property-alist nil)
(defvar-local xmp-dired-filter-file-predicates nil)
(defvar xmp-dired-filter-done nil)

;; For sorting

(defvar-local xmp-dired-sort-key nil)
(defvar xmp-dired-sort-done nil)

;; For display columns

(defvar-local xmp-dired-add-column-properties nil)

;;;;; Global Setup

(defvar xmp-dired-lsmod-global-mode nil)

(defun xmp-dired-lsmod-global-setup ()
  (unless xmp-dired-lsmod-global-mode
    (advice-add #'dired-insert-directory :around
                #'xmp-dired-lsmod-around-dired-insert-directory)
    (advice-add #'ls-lisp-handle-switches :around
                #'xmp-dired-lsmod-around-ls-lisp-handle-switches)
    (setq xmp-dired-lsmod-global-mode t)))

(defun xmp-dired-lsmod-global-teardown ()
  (when xmp-dired-lsmod-global-mode
    (advice-remove #'dired-insert-directory
                   #'xmp-dired-lsmod-around-dired-insert-directory)
    (advice-remove #'ls-lisp-handle-switches
                   #'xmp-dired-lsmod-around-ls-lisp-handle-switches)
    (setq xmp-dired-lsmod-global-mode nil)))

;;;;; Listing Modifications after `dired-insert-directory'

(defun xmp-dired-lsmod-around-dired-insert-directory (oldfun
                                                      dir switches
                                                      &optional file-list
                                                      wildcard hdr
                                                      &rest unknown-args)
  ":around advice for `dired-insert-directory'."
  (if (or xmp-dired-filter-property-alist
          xmp-dired-filter-file-predicates
          xmp-dired-sort-key
          xmp-dired-add-column-properties)
      (let* (;; Replace xmp-get-file-property
             (xmp-make-file-prop-sort-key-fun-get-property
              #'xmp-dired-lsmod-get-file-property)
             (xmp-dired-lsmod-files-properties-hash
              (make-hash-table :test 'equal))
             (xmp-dired-lsmod-required-properties
              (xmp-dired-lsmod-required-properties))
             ;; Processed flags
             (xmp-dired-filter-done (cons nil nil))
             (xmp-dired-sort-done (cons nil nil))
             ;; Call the original function
             (output-beg (point))
             (result (apply oldfun dir switches file-list wildcard hdr
                            unknown-args))
             (output-end (point-marker)))
        (unwind-protect
            (save-match-data
              (save-excursion
                ;; Filter (Reduce files before sorting)
                (when (and (or xmp-dired-filter-property-alist
                               xmp-dired-filter-file-predicates)
                           (not (car xmp-dired-filter-done)))
                  (xmp-dired-filter-file-lines dir output-beg output-end))
                ;; Sort
                (when (and xmp-dired-sort-key
                           (not (car xmp-dired-sort-done)))
                  (xmp-dired-sort-file-lines dir output-beg output-end))
                ;; Add column
                (when xmp-dired-add-column-properties
                  (xmp-dired-add-columns-to-file-lines dir
                                                       output-beg output-end))))
          (set-marker output-end nil))
        result)
    ;; Call the original function
    (apply oldfun dir switches file-list wildcard hdr unknown-args)))

(defun xmp-dired-lsmod-move-to-first-file-line (output-beg output-end)
  (goto-char output-beg)
  ;; Skip header (dir, wildard)
  (while (and (< (point) output-end) (null (dired-move-to-filename)))
    (forward-line))
  (when (< (point) output-end)
    (forward-line 0)
    t))

(defun xmp-dired-lsmod-get-filename (dir)
  (or
   (cl-letf (;; Don't use `dired-current-directory'.
             ((symbol-function 'dired-current-directory)
              (lambda (&optional _localp) dir))
             ;; `dired-subdir-alist' is not built at the time of
             ;; calling `dired-insert-directory'.
             (dired-subdir-alist nil))
     (dired-get-filename nil t)) ;; return nil if file is . or .. ?

   ;;The following code does not support various unescape and other conversions.
   (save-excursion
     (when-let* ((file-beg (dired-move-to-filename))
                 (file-end (dired-move-to-end-of-filename))
                 (file (buffer-substring-no-properties file-beg file-end)))
       (file-name-concat dir file)))))

;;;;; Listing Modifications before `ls-lisp-handle-switches'

(defun xmp-dired-lsmod-around-ls-lisp-handle-switches (oldfun
                                                       file-alist switches
                                                       &rest unknown-args)
  ;; Filter (Reduce files before sorting)
  (setq file-alist (xmp-dired-filter-before-ls-lisp-handle-switches file-alist))
  ;; Sort
  (seq-setq (file-alist switches)
            (xmp-dired-sort-before-ls-lisp-handle-switches file-alist switches))

  (apply oldfun file-alist switches unknown-args))

;;;;; Property Cache

(defun xmp-dired-lsmod-required-properties ()
  (seq-uniq
   (apply #'append
          (mapcar (lambda (sym)
                    (pcase sym
                      ((pred functionp) (funcall sym))
                      ((pred boundp) (symbol-value sym))))
                  xmp-dired-lsmod-required-properties-source))
   #'xmp-xml-ename-equal))
;; EXAMPLE: (xmp-dired-lsmod-required-properties)

(defun xmp-dired-lsmod-get-file-properties (file)
  (when xmp-dired-lsmod-files-properties-hash
    (car
     (or (gethash file xmp-dired-lsmod-files-properties-hash)
         (puthash file
                  (cons (xmp-get-file-properties
                         file xmp-dired-lsmod-required-properties)
                        nil)
                  xmp-dired-lsmod-files-properties-hash)))))

(defun xmp-dired-lsmod-get-file-property (file prop-ename)
  (when-let ((props (xmp-dired-lsmod-get-file-properties file)))
    (xmp-xml-ename-alist-get prop-ename props)))

;;;;; Filter

(defun xmp-dired-filter-required-properties ()
  "Return a list of expanded names of properties required for filtering."
  (mapcar #'car xmp-dired-filter-property-alist))

(defun xmp-dired-filter-match-properties-p (prop-ename-pvalue-alist)
  "Return non-nil if the file's properties satisfy the filter's extraction
conditions.

PROP-ENAME-PVALUE-ALIST is an alist of the properties and their values
that the file has. It must be the result of retrieving properties from
the file based on the list of property names returned by the function
`xmp-dired-filter-required-properties'."
  ;; TODO: Unify with `xmp-image-dired-filter-thumbnail-at-point-p'
  (cl-loop for (prop-ename . pred) in xmp-dired-filter-property-alist
           unless (funcall pred
                           (xmp-xml-ename-alist-get
                            prop-ename prop-ename-pvalue-alist))
           return nil
           finally return t))

(defun xmp-dired-filter-match-file-p (file)
  "Return non-nil if FILE satisfies the filter's extraction conditions."
  (and (seq-every-p (lambda (pred) (funcall pred file))
                    xmp-dired-filter-file-predicates)
       (xmp-dired-filter-match-properties-p
        (xmp-dired-lsmod-get-file-properties file))))

;;;;;; Filter after `dired-insert-directory'
;; Filtering when not using ls-lisp

(defun xmp-dired-filter-file-lines (dir output-beg output-end)
  (let (message-log-max) (message "Filtering files..."))
  (when (or xmp-dired-filter-property-alist
            xmp-dired-filter-file-predicates)
    (when (xmp-dired-lsmod-move-to-first-file-line output-beg output-end)
      (while (< (point) output-end)
        (let ((file (xmp-dired-lsmod-get-filename dir)))
          (if (or (null file)
                  (xmp-dired-filter-match-file-p file))
              (forward-line)
            (delete-line)))))))

;;;;;; Filter before `ls-lisp-handle-switches'
;; Filtering in ls-lisp is faster than filtering after
;; `dired-insert-directory', but ls-lisp is not used in all cases.

(defun xmp-dired-filter-before-ls-lisp-handle-switches (file-alist)
  ;; Note: `default-directory' is the target directory of the listing.
  ;;       See: `ls-lisp-insert-directory'
  (when xmp-dired-filter-property-alist
    (let (message-log-max) (message "Filtering files..."))
    (setq file-alist
          (seq-filter (lambda (file-attrs)
                        (xmp-dired-filter-match-file-p
                         (expand-file-name (car file-attrs))))
                      file-alist))
    (message nil)
    ;; Mark filtered (Suppress filtering after `dired-insert-directory')
    (when xmp-dired-filter-done
      (setcar xmp-dired-filter-done t)))
  file-alist)

;;;;;; Filter Commands

(defcustom xmp-dired-filter-sidecar-auto-hide t
  "Non-nil means hide sidecar files when filtering by properties."
  :group 'xmp
  :type 'boolean)

;;;###autoload
(defun xmp-dired-filter-clear ()
  (interactive nil dired-mode)
  (setq xmp-dired-filter-property-alist nil)
  (when xmp-dired-filter-sidecar-auto-hide
    (xmp-dired-filter-show-sidecar t))
  (revert-buffer))

(defun xmp-dired-filter-set (prop-ename pred)
  (xmp-dired-lsmod-global-setup)
  (if pred
      (setf (xmp-xml-ename-alist-get prop-ename
                                     xmp-dired-filter-property-alist)
            pred)
    (setf (xmp-xml-ename-alist-get prop-ename
                                   xmp-dired-filter-property-alist nil t)
          nil))
  (when xmp-dired-filter-sidecar-auto-hide
    (if xmp-dired-filter-property-alist
        (xmp-dired-filter-hide-sidecar t)
      (xmp-dired-filter-show-sidecar t))))

;;;###autoload
(defun xmp-dired-filter-property (prop-ename pred &optional arg)
  "Display files for which the value of the property specified by
PROP-ENAME satisfies the predicate PRED.

If the prefix argument is - or 0, remove the filter for the property. If
the prefix argument is any other non-nil value, invert the specified
condition."
  (interactive
   (xmp-filter-read-property-condition)
   dired-mode)

  (xmp-dired-filter-set prop-ename
                        (xmp-filter-gen-property-predicate pred arg))
  (revert-buffer))

;;;###autoload
(defun xmp-dired-filter-rating (condition &optional arg)
  (interactive (xmp-filter-read-rating-condition) dired-mode)
  (xmp-dired-filter-set xmp-xmp:Rating
                        (xmp-filter-gen-rating-predicate condition arg))
  (revert-buffer))

;;;###autoload
(defun xmp-dired-filter-label (label &optional arg)
  (interactive (xmp-filter-read-label-condition) dired-mode)
  (xmp-dired-filter-set xmp-xmp:Label
                        (xmp-filter-gen-label-predicate label arg))
  (revert-buffer))

;;;###autoload
(defun xmp-dired-filter-subjects (subjects &optional arg)
  (interactive (xmp-filter-read-subjects-condition) dired-mode)
  (xmp-dired-filter-set xmp-dc:subject
                        (xmp-filter-gen-subjects-predicate subjects arg))
  (revert-buffer))

;;;###autoload
(defun xmp-dired-filter-title (pred &optional arg)
  (interactive (xmp-filter-read-property-condition xmp-dc:title) dired-mode)
  (xmp-dired-filter-set xmp-dc:title
                        (xmp-filter-gen-property-predicate pred arg))
  (revert-buffer))

;;;###autoload
(defun xmp-dired-filter-description (pred &optional arg)
  (interactive (xmp-filter-read-property-condition xmp-dc:description)
               dired-mode)
  (xmp-dired-filter-set xmp-dc:description
                        (xmp-filter-gen-property-predicate pred arg))
  (revert-buffer))

;;;###autoload
(defun xmp-dired-filter-creators (pred &optional arg)
  (interactive (xmp-filter-read-property-condition xmp-dc:creator) dired-mode)
  (xmp-dired-filter-set xmp-dc:creator
                        (xmp-filter-gen-property-predicate pred arg))
  (revert-buffer))

;;;###autoload
(defun xmp-dired-filter-toggle-sidecar ()
  (interactive)
  (xmp-dired-lsmod-global-setup)
  (if (memq #'xmp-not-sidecar-file-p xmp-dired-filter-file-predicates)
      (setq xmp-dired-filter-file-predicates
            (delq #'xmp-not-sidecar-file-p xmp-dired-filter-file-predicates))
    (push #'xmp-not-sidecar-file-p xmp-dired-filter-file-predicates))
  (revert-buffer))

;;;###autoload
(defun xmp-dired-filter-hide-sidecar (&optional no-revert)
  (interactive)
  (xmp-dired-lsmod-global-setup)
  (unless (memq #'xmp-not-sidecar-file-p xmp-dired-filter-file-predicates)
    (push #'xmp-not-sidecar-file-p xmp-dired-filter-file-predicates))
  (unless no-revert
    (revert-buffer)))

;;;###autoload
(defun xmp-dired-filter-show-sidecar (&optional no-revert)
  (interactive)
  (setq xmp-dired-filter-file-predicates
        (delq #'xmp-not-sidecar-file-p xmp-dired-filter-file-predicates))
  (unless no-revert
    (revert-buffer)))


;;;;; Sort

(defun xmp-dired-sort-required-properties ()
  "Return a list of expanded names of properties required for sorting."
  (when xmp-dired-sort-key
    (when-let ((prop-ename (xmp-file-property-sort-key-ename xmp-dired-sort-key)))
      (list prop-ename))))

;;;;;; Sort after `dired-insert-directory'
;; Sorting when not using ls-lisp

(defun xmp-dired-sort-file-lines (dir output-beg output-end)
  "A file sorting operation that is run after `dired-insert-directory'."
  (when (xmp-dired-lsmod-move-to-first-file-line output-beg output-end)
    (save-restriction
      (narrow-to-region (point) output-end)
      (let ((dir (expand-file-name (file-name-as-directory dir)))
            (fun-key (xmp-file-property-sort-key-fun-key xmp-dired-sort-key))
            (fun-less (xmp-file-property-sort-key-fun-less xmp-dired-sort-key)))
        (sort-subr nil
                   #'forward-line #'end-of-line
                   (lambda ()
                     (when-let ((file (xmp-dired-lsmod-get-filename dir)))
                       (funcall fun-key file)))
                   nil fun-less)))))

;;;;;; Sort before `ls-lisp-handle-switches'
;; Sorting in ls-lisp is faster than sorting after
;; `dired-insert-directory', but ls-lisp is not used in all cases.

(defun xmp-dired-sort-before-ls-lisp-handle-switches (file-alist switches)
  (when xmp-dired-sort-key
    ;; Sort FILE-ALIST
    (setq file-alist (xmp-dired-sort-ls-lisp-file-alist file-alist))
    ;; Add unsorted option (Suppress sorting in `ls-lisp-handle-switches')
    (setq switches (cons ?U switches))
    ;; Mark sorted (Suppress sorting after `dired-insert-directory')
    (when xmp-dired-sort-done
      (setcar xmp-dired-sort-done t)))
  (list file-alist switches))

(defun xmp-dired-sort-ls-lisp-file-alist (file-alist)
  ;; Note: `default-directory' is the target directory of the listing.
  ;;       See: `ls-lisp-insert-directory'
  (let* ((fun-key (xmp-file-property-sort-key-fun-key xmp-dired-sort-key))
         (fun-less (xmp-file-property-sort-key-fun-less xmp-dired-sort-key))
         (_ (let (message-log-max) (message "Retrieving file properties...")))
         (files-keys (mapcar
                      (lambda (file-attrs)
                        (cons
                         file-attrs
                         (funcall fun-key (expand-file-name (car file-attrs)))))
                      file-alist))
         (_ (let (message-log-max) (message "Sorting files...")))
         (sorted-list (sort files-keys
                            :key #'cdr
                            :lessp fun-less
                            :reverse t)))
    ;; Discard key
    (cl-loop for x on sorted-list
             do (setcar x (caar x)))
    (message nil)
    sorted-list))

;;;;;; Sort Commands

;; TODO: Support multiple properties as keys

;;;###autoload
(defun xmp-dired-sort-by-property (prop-ename &optional reverse)
  "Sort files in Dired by the XMP property specified by PROP-ENAME.
If the prefix argument or REVERSE is non-nil, sort in reverse order.
If PROP-ENAME is nil, call `xmp-dired-sort-clear'."
  (interactive
   (list (xmp-read-property-ename (xmp-msg "Key property: "))
         current-prefix-arg))

  (if (null prop-ename)
      (xmp-dired-sort-clear)
    (xmp-dired-lsmod-global-setup)
    (setq-local xmp-dired-sort-key
                (xmp-make-file-property-sort-key prop-ename reverse))
    (revert-buffer)))

;;;###autoload
(defun xmp-dired-sort-clear ()
  "Clear the sorting effect set by `xmp-dired-sort-by-property'."
  (interactive)
  (setq-local xmp-dired-sort-key nil)
  (revert-buffer))

;;;###autoload
(defun xmp-dired-sort-by-rating (&optional reverse)
  (interactive "P")
  (xmp-dired-sort-by-property xmp-xmp:Rating reverse))

;;;###autoload
(defun xmp-dired-sort-by-label (&optional reverse)
  (interactive "P")
  (xmp-dired-sort-by-property xmp-xmp:Label reverse))

;;;###autoload
(defun xmp-dired-sort-by-subjects (&optional reverse)
  (interactive "P")
  (xmp-dired-sort-by-property xmp-dc:subject reverse))

;;;###autoload
(defun xmp-dired-sort-by-title (&optional reverse)
  (interactive "P")
  (xmp-dired-sort-by-property xmp-dc:title reverse))

;;;###autoload
(defun xmp-dired-sort-by-description (&optional reverse)
  (interactive "P")
  (xmp-dired-sort-by-property xmp-dc:description reverse))

;;;###autoload
(defun xmp-dired-sort-by-creators (&optional reverse)
  (interactive "P")
  (xmp-dired-sort-by-property xmp-dc:creator reverse))

;;;;; Columns

(defcustom xmp-dired-display-columns-max-width 40
  "The maximum text width for each property value displayed as a column in
Dired."
  :group 'xmp
  :type 'integer)

(defun xmp-dired-add-columns-to-file-lines (dir output-beg output-end)
  (save-match-data
    (save-excursion
      (xmp-dired-add-column--insert
       output-beg output-end
       (xmp-dired-add-column--collect-lines-columns dir output-beg output-end))
      (message nil))))

(defun xmp-dired-add-column--collect-lines-columns (dir output-beg output-end)
  (let (message-log-max) (message "Retrieving file properties..."))
  (when (xmp-dired-lsmod-move-to-first-file-line output-beg output-end)
    (cl-loop while (< (point) output-end)
             for file = (xmp-dired-lsmod-get-filename dir)
             collect
             (when file
               (let ((prop-alist (xmp-dired-lsmod-get-file-properties file)))
                 (cl-loop
                  for prop-ename in xmp-dired-add-column-properties
                  for value-str
                  = (if (xmp-sidecar-file-p file)
                        "" ;; Hide sidecar file's properties
                      (xmp-dired-add-column--stringize-property
                       prop-ename
                       (xmp-xml-ename-alist-get prop-ename prop-alist)))
                  collect value-str)))
             do (forward-line))))

(defun xmp-dired-add-column--stringize-property (prop-ename pvalue)
  (truncate-string-to-width
   (if pvalue
       (replace-regexp-in-string
        "[\n\r\t]+" " "
        (or (xmp-pvalue-to-display-string pvalue prop-ename) ""))
     "")
   xmp-dired-display-columns-max-width nil nil t))

(defun xmp-dired-add-column--max-widths (lines-columns)
  (let ((max-widths (make-list (length xmp-dired-add-column-properties) 0)))
    (dolist (columns lines-columns)
      (when columns
        (cl-loop for column in columns
                 for widths on max-widths
                 for column-w = (string-width column)
                 if (< (car widths) column-w) do (setcar widths column-w))))
    max-widths))

(defun xmp-dired-add-column--insert (output-beg output-end lines-columns)
  (when (xmp-dired-lsmod-move-to-first-file-line output-beg output-end)
    (let (message-log-max) (message "Inserting columns..."))
    (let ((max-widths (xmp-dired-add-column--max-widths lines-columns)))
      (dolist (columns lines-columns)
        (when columns
          (cl-loop for column in columns
                   for max-width in max-widths
                   for column-w = (string-width column)
                   when (dired-move-to-filename)
                   do
                   (backward-char)
                   (insert " " column)
                   (insert-char ?  (- max-width column-w))))
        (forward-line)))))

;;;###autoload
(defun xmp-dired-add-column (prop-ename)
  (interactive
   (list (xmp-read-property-ename (xmp-msg "Add column property: ")))
   dired-mode)

  (xmp-dired-lsmod-global-setup)
  (setq xmp-dired-add-column-properties
        (nconc
         (seq-remove (lambda (x) (xmp-xml-ename-equal x prop-ename))
                     xmp-dired-add-column-properties)
         (list prop-ename)))
  (revert-buffer))

;;;###autoload
(defun xmp-dired-remove-column (prop-ename)
  (interactive
   (list (xmp-read-property-ename (xmp-msg "Remove column property: ")))
   dired-mode)
  (setq xmp-dired-add-column-properties
        (seq-remove (lambda (x) (xmp-xml-ename-equal x prop-ename))
                    xmp-dired-add-column-properties))
  (revert-buffer))

;;;###autoload
(defun xmp-dired-remove-all-columns ()
  (interactive)
  (setq xmp-dired-add-column-properties nil)
  (revert-buffer))

;;;###autoload
(defun xmp-dired-toggle-column (prop-ename)
  (interactive
   (list (xmp-read-property-ename (xmp-msg "Toggle column property: ")))
   dired-mode)
  (if (xmp-xml-ename-member prop-ename xmp-dired-add-column-properties)
      (xmp-dired-remove-column prop-ename)
    (xmp-dired-add-column prop-ename)))

;;;###autoload
(defun xmp-dired-toggle-column-rating ()
  (interactive)
  (xmp-dired-toggle-column xmp-xmp:Rating))

;;;###autoload
(defun xmp-dired-toggle-column-label ()
  (interactive)
  (xmp-dired-toggle-column xmp-xmp:Label))

;;;###autoload
(defun xmp-dired-toggle-column-subjects ()
  (interactive)
  (xmp-dired-toggle-column xmp-dc:subject))

;;;###autoload
(defun xmp-dired-toggle-column-title ()
  (interactive)
  (xmp-dired-toggle-column xmp-dc:title))

;;;###autoload
(defun xmp-dired-toggle-column-description ()
  (interactive)
  (xmp-dired-toggle-column xmp-dc:description))

;;;###autoload
(defun xmp-dired-toggle-column-creators ()
  (interactive)
  (xmp-dired-toggle-column xmp-dc:creator))

(provide 'xmp-image-dired)
;;; xmp-image-dired.el ends here
