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

;;;; Sort

(defvar-local xmp-dired-sort-fun-key-less nil)
(defvar xmp-dired-sort-sorted nil)

(defun xmp-dired-sort-global-setup ()
  (advice-add #'dired-insert-directory :around
              #'xmp-dired-sort-around-dired-insert-directory)
  (advice-add #'ls-lisp-handle-switches :around
              #'xmp-dired-sort-around-ls-lisp-handle-switches))

(defun xmp-dired-sort-global-teardown ()
  (advice-remove #'dired-insert-directory
                 #'xmp-dired-sort-around-dired-insert-directory)
  (advice-remove #'ls-lisp-handle-switches
                 #'xmp-dired-sort-around-ls-lisp-handle-switches))

;;;;; Sort after `dired-insert-directory'

;; Sorting when not using ls-lisp

(defun xmp-dired-sort-around-dired-insert-directory (oldfun
                                                     dir switches
                                                     &optional file-list
                                                     wildcard hdr
                                                     &rest unknown-args)
  ":around advice for `dired-insert-directory'."
  (let* ((output-beg (point))
         (xmp-dired-sort-sorted (cons nil nil))
         (result (apply oldfun dir switches file-list wildcard hdr
                        unknown-args))
         (output-end (point)))
    (when (and xmp-dired-sort-fun-key-less
               (not (car xmp-dired-sort-sorted)))
      (xmp-dired-sort-after-dired-insert-directory
       dir output-beg output-end))
    result))

(defun xmp-dired-sort-after-dired-insert-directory (dir
                                                    output-beg output-end)
  "A file sorting operation that is run after `dired-insert-directory'."
  (message "Sort after dired-insert-directory")
  (save-match-data
    (save-excursion
      (goto-char output-beg)
      ;; Skip header (dir, wildard)
      (while (and (< (point) output-end) (null (dired-move-to-filename)))
        (forward-line))
      (when (< (point) output-end)
        (forward-line 0)
        ;; Sort
        (save-restriction
          (narrow-to-region (point) output-end)
          (xmp-dired-sort-inserted-file-lines
           dir
           (car xmp-dired-sort-fun-key-less)
           (cdr xmp-dired-sort-fun-key-less)))))))

(defun xmp-dired-sort-inserted-file-lines (dir fun-file-to-key fun-less-keys)
  (setq dir (expand-file-name (file-name-as-directory dir)))
  (sort-subr nil
             #'forward-line #'end-of-line
             (lambda ()
               (when-let ((file (xmp-dired-sort-get-filename dir)))
                 (funcall fun-file-to-key file)))
             nil fun-less-keys))

(defun xmp-dired-sort-get-filename (dir)
  (or
   (cl-letf (;; Don't use `dired-current-directory'.
             ((symbol-function 'dired-current-directory)
              (lambda (&optional _localp) dir))
             ;; `dired-subdir-alist' is not built at the time of
             ;; calling `dired-insert-directory'.
             (dired-subdir-alist nil))
     (dired-get-filename nil t)) ;; return nil if file is . or ..

   ;;The following code does not support various unescape and other conversions.
   (save-excursion
     (when-let* ((file-beg (dired-move-to-filename))
                 (file-end (dired-move-to-end-of-filename))
                 (file (buffer-substring-no-properties file-beg file-end)))
       (file-name-concat dir file)))))

;;;;; Sort before `ls-lisp-handle-switches'

;; Sorting in ls-lisp is faster than sorting after
;; `dired-insert-directory', but ls-lisp is not used in all cases.

(defun xmp-dired-sort-around-ls-lisp-handle-switches (oldfun
                                                      file-alist switches
                                                      &rest unknown-args)
  (when xmp-dired-sort-fun-key-less
    ;; Sort FILE-ALIST
    (setq file-alist (xmp-dired-sort-before-ls-lisp-handle-switches file-alist))
    ;; Add unsorted option (Suppress sorting in `ls-lisp-handle-switches')
    (setq switches (cons ?U switches))
    ;; Mark sorted (Suppress sorting after `dired-insert-directory')
    (when xmp-dired-sort-sorted
      (setcar xmp-dired-sort-sorted t)))

  (apply oldfun file-alist switches unknown-args))

(defun xmp-dired-sort-before-ls-lisp-handle-switches (file-alist)
  ;; Note: `default-directory' is the target directory of the listing.
  ;;       See: `ls-lisp-insert-directory'
  (let* ((fun-key (car xmp-dired-sort-fun-key-less))
         (fun-less (cdr xmp-dired-sort-fun-key-less))
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
    (let (message-log-max) (message ""))
    sorted-list))

;;;;; Sort commands

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
    (xmp-dired-sort-global-setup)
    (setq-local xmp-dired-sort-fun-key-less
                (xmp-make-file-prop-sort-funs-key-and-less prop-ename reverse))
    (revert-buffer)))

;;;###autoload
(defun xmp-dired-sort-clear ()
  "Clear the sorting effect set by `xmp-dired-sort-by-property'."
  (interactive)
  (setq-local xmp-dired-sort-fun-key-less nil)
  (revert-buffer))

(provide 'xmp-image-dired)
;;; xmp-image-dired.el ends here
