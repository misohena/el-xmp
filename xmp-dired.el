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

(defun xmp-dired--mark-if (pred unflag-p &optional msg)
  (let ((dired-marker-char (if unflag-p ?\s dired-marker-char))
        (msg (or msg "matching file")))
    (dired-mark-if
     (and (not (looking-at-p dired-re-dot))
          (not (eolp)) ; empty line
          (let ((fn (dired-get-filename nil t)))
            (when (and fn
                       (not (when-let ((ext (file-name-extension fn)))
                              (string-equal-ignore-case ext "xmp")))
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

;;;; Change properties

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
    (dolist (file files)
      (xmp-rate-file file rating))
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
    (dolist (file files)
      (xmp-set-file-label file label))
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
    (dolist (file files)
      (xmp-set-file-subjects file subjects))
    (dired-post-do-command)))

;;;###autoload
(defun xmp-dired-do-add-subjects (&optional arg)
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (subjects (xmp-read-file-subjects
                    (xmp-msg "Add %%s to subjects of %s.\nSubject to toggle (empty to end): ")
                    (dired-mark-prompt arg files)
                    nil)))
    (dolist (file files)
      (xmp-set-file-subjects
       file
       (seq-union (xmp-get-file-subjects file) subjects)))
    (dired-post-do-command)))

;;;###autoload
(defun xmp-dired-do-remove-subjects (&optional arg)
  (interactive "P" dired-mode)
  (let* ((files (dired-get-marked-files t arg nil nil t))
         (subjects (xmp-read-file-subjects
                    (xmp-msg "Remove %%s from subjects of %s.\nSubject to toggle (empty to end): ")
                    (dired-mark-prompt arg files)
                    nil)))
    (dolist (file files)
      (xmp-set-file-subjects
       file
       (seq-difference (xmp-get-file-subjects file) subjects)))
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
    (dolist (file files)
      (xmp-set-file-title file title))
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
    (dolist (file files)
      (xmp-set-file-description file description))
    (dired-post-do-command)))



(provide 'xmp-image-dired)
;;; xmp-image-dired.el ends here
