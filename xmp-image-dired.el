;;; xmp-image-dired.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: multimedia, files

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

;; This file adds XMP-related features to image-dired.

;;; Code:

(require 'image-dired)
(require 'xmp-commands)

;;;; Read metadata and set it to thumbnail

(defconst xmp-image-dired-metadata-properties
  '(("http://ns.adobe.com/xap/1.0/" "Rating")
    ("http://ns.adobe.com/xap/1.0/" "Label")
    ;;("http://ns.adobe.com/xap/1.0/" "CreateDate")
    ;;("http://purl.org/dc/elements/1.1/" "title")
    ;;("http://purl.org/dc/elements/1.1/" "description")
    ("http://purl.org/dc/elements/1.1/" "subject")
    ;;("http://purl.org/dc/elements/1.1/" "creator")
    ))

(defun xmp-image-dired-metadata-properties ()
  (cl-loop for (ns-name local-name)
           in xmp-image-dired-metadata-properties
           collect (xmp-xml-ename (xmp-xml-ns-name ns-name) local-name)))

(defun xmp-image-dired-get-metadata-at-point ()
  (when (image-dired-image-at-point-p)
    ;; To cache metadata as text properties, use thefollowing.
    ;; However, it must be updated when the metadata changes.
    ;; (unless (get-text-property (point) 'xmp-properties)
    ;;   (let* ((file (get-text-property (point) 'original-file-name))
    ;;          (props (xmp-enumerate-file-properties
    ;;                  file (xmp-image-dired-metadata-properties))))
    ;;     (put-text-property (point) (1+ (point)) 'xmp-properties props)))
    ;; (get-text-property (point) 'xmp-properties)

    ;; It's simpler to retrieve it every time, and since caching works
    ;; it shouldn't be that slow.
    (xmp-enumerate-file-properties
     (get-text-property (point) 'original-file-name)
     (xmp-image-dired-metadata-properties))))


;;;; Filter thumbnails
;;;;; Override the line-up function to support invisible thumbnails

(defconst xmp-image-dired-invisible-display "")

;; Copy from Emacs 30.0.91
(defun xmp-image-dired-line-up ()
  "Line up thumbnails according to `image-dired-thumbs-per-row'.
See also `image-dired-line-up-dynamic'."
  (interactive nil image-dired-thumbnail-mode)
  (let ((inhibit-read-only t))
    ;; Remove all whitespaces
    (goto-char (point-min))
    (while (and (not (image-dired-image-at-point-p))
                (not (eobp)))
      (delete-char 1))
    (while (not (eobp))
      (forward-char)
      (while (and (not (image-dired-image-at-point-p))
                  (not (eobp)))
        (delete-char 1)))

    ;; Insert whitespaces
    (goto-char (point-min))
    (let ((seen 0)
          (thumb-prev-pos 0)
          (thumb-width-chars
           (ceiling (/ (+ (* 2 image-dired-thumb-relief)
                          (* 2 image-dired-thumb-margin)
                          (image-dired--thumb-size))
                       (float (frame-char-width))))))
      ;; ------------------------------------------------------------
      ;; Beginning of changes

      ;; Note: Two characters (thumbnail image + space) are required
      ;; for each thumbnail. Because there are some places where
      ;; (forward-char 2) is used to visit thumbnails.

      ;; Note: there must not be invisible white space immediately
      ;; before the visible thumbnail, because moving point to the
      ;; left would move it to the left of that white space.
      (while (not (eobp))
        (if (not (xmp-image-dired-thumbnail-visible-at-point-p))
            ;; If the thumbnail is invisible, simply skip it
            (progn
              (unless (bobp)
                (insert (propertize " " 'display "" 'invisible t)))
              (forward-char))
          ;; the thumbnail is visible
          (unless (bobp)
            (if (and (= seen image-dired-thumbs-per-row))
                (progn
                  (insert "\n")
                  (setq seen 0)
                  (setq thumb-prev-pos 0))
              (insert (propertize " " 'display `(space :align-to ,thumb-prev-pos)))))
          (forward-char)
          (cl-incf thumb-prev-pos thumb-width-chars)
          (cl-incf seen)))
      (unless (bobp)
        (insert (propertize " " 'display "" 'invisible t)))
      ;; End of changes
      ;; ------------------------------------------------------------
      )
    (goto-char (point-min))))

(advice-add 'image-dired-line-up :override 'xmp-image-dired-line-up)
;;(advice-remove 'image-dired-line-up 'xmp-image-dired-line-up)


;;;;; Override navigation function

;; Copy from Emacs 30.0.91
(defun xmp-image-dired-forward-image (&optional arg wrap-around)
  "Move to next image in the thumbnail buffer.
Optional prefix ARG says how many images to move; the default is
one image.  Negative means move backwards.
On reaching end or beginning of buffer, stop and show a message.

If optional argument WRAP-AROUND is non-nil, wrap around: if
point is on the last image, move to the last one and vice versa."
  (interactive "p" image-dired-thumbnail-mode)
  (setq arg (or arg 1))
  (let (pos)
    ;; +Beginning of changes
    ;; Skip invisible
    (while (and (not (if (> arg 0) (eobp) (bobp)))
                (get-text-property (point) 'invisible))
      (forward-char (if (> arg 0) 1 -1)))
    ;; +End of changes

    (dotimes (_ (abs arg))
      (if (and (not (if (> arg 0) (eobp) (bobp)))
               (save-excursion
                 (forward-char (if (> arg 0) 1 -1))
                 (while (and (not (if (> arg 0) (eobp) (bobp)))
                             ;; +Beginning of changes
                             (not (xmp-image-dired-thumbnail-visible-at-point-p))
                             ;; +End of changes
                             )
                   (forward-char (if (> arg 0) 1 -1)))
                 (setq pos (point))
                 ;; +Beginning of changes
                 (xmp-image-dired-thumbnail-visible-at-point-p)
                 ;; +End of changes
                 ))
          (goto-char pos)
        (if wrap-around
            (goto-char (if (> arg 0)
                           (point-min)
                         ;; There are two spaces after the last image.
                         (- (point-max) 2)))
          (message "At %s image" (if (> arg 0) "last" "first"))))))
  (image-dired--update-header-line)
  (when image-dired-track-movement
    (image-dired-track-original-file)))

(advice-add 'image-dired-forward-image :override 'xmp-image-dired-forward-image)
;;(advice-remove 'image-dired-forward-image 'xmp-image-dired-forward-image)


;; Copy from Emacs 30.0.91
(defun xmp-image-dired--movement-ensure-point-pos (&optional reverse)
  "Ensure point is on an image."
  (while (and (not
               ;; +Beginning of changes
               (xmp-image-dired-thumbnail-visible-at-point-p)
               ;; +End of changes
               )
              (not (if reverse (bobp) (eobp))))
    (forward-char (if reverse -1 1))))

(advice-add 'image-dired--movement-ensure-point-pos :override 'xmp-image-dired--movement-ensure-point-pos)
;;(advice-remove 'image-dired--movement-ensure-point-pos 'xmp-image-dired--movement-ensure-point-pos)

;;;;; Predicate to get thumbnail visibility state

(defun xmp-image-dired-thumbnail-visible-at-point-p ()
  (and (image-dired-image-at-point-p)
       (eq (get-text-property (point) 'invisible) nil)))

;;;;; Change thumbnail visibilities

(defun xmp-image-dired-set-visibility-at-point (visible)
  (when (image-dired-image-at-point-p)
    ;; Backup display property.
    ;; TODO: There may be problems in the future. The current use of
    ;; the image-dired display property is problematic and may be
    ;; fixed in the future. At that point, it may not make sense to
    ;; back up the display property of thumbnails that have not yet
    ;; been generated.
    (unless (get-text-property (point) 'xmp-display-backup)
      (put-text-property (point) (1+ (point)) 'xmp-display-backup
                         (get-text-property (point) 'display)))
    ;; Change text property.
    (if visible
        (progn
          (put-text-property (point) (1+ (point)) 'display
                             (get-text-property (point) 'xmp-display-backup))
          (put-text-property (point) (1+ (point)) 'invisible
                             nil))
      (put-text-property (point) (1+ (point))
                         'display
                         xmp-image-dired-invisible-display)
      (put-text-property (point) (1+ (point))
                         'invisible
                         t))))

;;;;; Filter thumbnails

(defun xmp-image-dired-filter-thumbnails ()
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (not (eobp))
        (when (image-dired-image-at-point-p)
          (xmp-image-dired-set-visibility-at-point
           (xmp-image-dired-filter-thumbnail-at-point-p)))
        (forward-char)))
    ;; Update spaces and line breaks
    (image-dired--line-up-with-method)))

(defvar-local xmp-image-dired-filter-alist nil
  "An alist of property expanded names and predicates. If the predicate is
nil, it is considered a match. If all elements match, the whole is
considered a match.")

;;;###autoload
(defun xmp-image-dired-filter-clear ()
  (interactive nil image-dired-thumbnail-mode)
  (setq xmp-image-dired-filter-alist nil)
  (xmp-image-dired-filter-thumbnails))

(defun xmp-image-dired-filter-thumbnail-at-point-p ()
  (when (image-dired-image-at-point-p)
    (let ((props (xmp-image-dired-get-metadata-at-point)))
      (seq-every-p
       (lambda (filter)
         (if-let ((pred (cdr filter)))
             (funcall pred (xmp-xml-ename-alist-get (car filter) props))
           t))
       xmp-image-dired-filter-alist))))

;;;###autoload
(defun xmp-image-dired-filter-rating (condition)
  (interactive
   (list
    (let ((input (read-string (xmp-msg "Filter rating (e.g. 1 3 >=5): "))))
      (if (string-empty-p input)
          nil
        input)))
   image-dired-thumbnail-mode)
  (setf (xmp-xml-ename-alist-get xmp-xmp:Rating xmp-image-dired-filter-alist)
        (and condition
             (lambda (v) (xmp-rating-match-p v condition))))
  (xmp-image-dired-filter-thumbnails))

;;;###autoload
(defun xmp-image-dired-filter-label (label)
  (interactive
   (let ((input (completing-read (xmp-msg "Filter label: ")
                                 (mapcar #'car xmp-label-strings))))
     (list
      (if (string-empty-p input)
          nil
        input)))
   image-dired-thumbnail-mode)
  (setf (xmp-xml-ename-alist-get xmp-xmp:Label xmp-image-dired-filter-alist)
        (and label
             (lambda (v) (equal (xmp-pvalue-as-text v) label))))
  (xmp-image-dired-filter-thumbnails))

;;;###autoload
(defun xmp-image-dired-filter-subjects (subjects)
  (interactive
   (list
    (xmp-read-text-list
     (xmp-msg "Filter subjects (AND): %s\nSubject to toggle (empty to end): ")
     nil
     xmp-read-subjects-candidates
     'xmp-read-subjects--hist))
   image-dired-thumbnail-mode)
  (setf (xmp-xml-ename-alist-get xmp-dc:subject xmp-image-dired-filter-alist)
        (and subjects
             (lambda (v)
               (seq-every-p
                (lambda (sbj) (member sbj (xmp-pvalue-as-text-list v)))
                subjects))))
  (xmp-image-dired-filter-thumbnails))

;;;; Change properties

(defun xmp-image-dired-get-marked-files ()
  (let (files)
    (image-dired--with-marked
     (push (image-dired-original-file-name) files))
    (unless files
      (user-error "No files specified"))
    (nreverse files)))

;;;###autoload
(defun xmp-image-dired-do-rate ()
  (interactive nil image-dired-thumbnail-mode)
  (let* ((files (xmp-image-dired-get-marked-files))
         (rating (xmp-read-file-rating files
                                       ;; current value
                                       (unless (cdr files)
                                         (xmp-get-file-rating (car files))))))
    (dolist (file files)
      (xmp-rate-file file rating))))

;;;###autoload
(defun xmp-image-dired-do-set-label ()
  (interactive nil image-dired-thumbnail-mode)
  (let* ((files (xmp-image-dired-get-marked-files))
         (label (xmp-read-file-label
                 nil files
                 ;; current value
                 (unless (cdr files)
                   (xmp-get-file-label (car files))))))
    (dolist (file files)
      (xmp-set-file-label file label))))

;;;###autoload
(defun xmp-image-dired-do-set-subjects ()
  (interactive nil image-dired-thumbnail-mode)
  (let* ((files (xmp-image-dired-get-marked-files))
         (subjects (xmp-read-file-subjects
                    nil files
                    ;; current value
                    (unless (cdr files)
                      (xmp-get-file-subjects (car files))))))
    (dolist (file files)
      (xmp-set-file-subjects file subjects))))

;;;###autoload
(defun xmp-image-dired-do-add-subjects ()
  (interactive nil image-dired-thumbnail-mode)
  (let* ((files (xmp-image-dired-get-marked-files))
         (subjects (xmp-read-file-subjects
                    (xmp-msg "Add %%s to subjects of %s.\nSubject to toggle (empty to end): ")
                    files
                    nil)))
    (dolist (file files)
      (xmp-set-file-subjects
       file
       (seq-union (xmp-get-file-subjects file) subjects)))))

;;;###autoload
(defun xmp-image-dired-do-remove-subjects ()
  (interactive nil image-dired-thumbnail-mode)
  (let* ((files (xmp-image-dired-get-marked-files))
         (subjects (xmp-read-file-subjects
                    (xmp-msg "Remove %%s from subjects of %s.\nSubject to toggle (empty to end): ")
                    files
                    nil)))
    (dolist (file files)
      (xmp-set-file-subjects
       file
       (seq-difference (xmp-get-file-subjects file) subjects)))))

;;;###autoload
(defun xmp-image-dired-do-set-title ()
  (interactive nil image-dired-thumbnail-mode)
  (let* ((files (xmp-image-dired-get-marked-files))
         (title (xmp-read-file-title
                 nil files
                 ;; current value
                 (unless (cdr files)
                   (xmp-get-file-title-alist (car files))))))
    (dolist (file files)
      (xmp-set-file-title file title))))

;;;###autoload
(defun xmp-image-dired-do-set-description ()
  (interactive nil image-dired-thumbnail-mode)
  (let* ((files (xmp-image-dired-get-marked-files))
         (description (xmp-read-file-description
                       nil
                       files
                       ;; current value
                       (unless (cdr files)
                         (xmp-get-file-description-alist (car files))))))
    (dolist (file files)
      (xmp-set-file-description file description))))

(provide 'xmp-image-dired)
;;; xmp-image-dired.el ends here
