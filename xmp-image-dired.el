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

;; (defconst xmp-image-dired-metadata-properties
;;   '(("http://ns.adobe.com/xap/1.0/" "Rating")
;;     ("http://ns.adobe.com/xap/1.0/" "Label")
;;     ;;("http://ns.adobe.com/xap/1.0/" "CreateDate")
;;     ;;("http://purl.org/dc/elements/1.1/" "title")
;;     ;;("http://purl.org/dc/elements/1.1/" "description")
;;     ("http://purl.org/dc/elements/1.1/" "subject")
;;     ;;("http://purl.org/dc/elements/1.1/" "creator")
;;     ))

;; (defun xmp-image-dired-metadata-properties ()
;;   (cl-loop for (ns-name local-name)
;;            in xmp-image-dired-metadata-properties
;;            collect (xmp-xml-ename (xmp-xml-ns-name ns-name) local-name)))

(defun xmp-image-dired-get-metadata-at-point ()
  (when (image-dired-image-at-point-p)
    ;; To cache metadata as text properties, use thefollowing.
    ;; However, it must be updated when the metadata changes.
    ;; (unless (get-text-property (point) 'xmp-properties)
    ;;   (let* ((file (get-text-property (point) 'original-file-name))
    ;;          (props (xmp-get-file-properties
    ;;                  file (xmp-image-dired-metadata-properties))))
    ;;     (put-text-property (point) (1+ (point)) 'xmp-properties props)))
    ;; (get-text-property (point) 'xmp-properties)

    ;; It's simpler to retrieve it every time, and since caching works
    ;; it shouldn't be that slow.
    (when-let ((required-props
                (xmp-image-dired-filter-required-properties)))
      (xmp-get-file-properties
       (get-text-property (point) 'original-file-name)
       ;;(xmp-image-dired-metadata-properties)
       required-props))))


;;;; Filter thumbnails
;;;;; Override the line-up function to support invisible thumbnails

(defconst xmp-image-dired-invisible-display "")

(defun xmp-image-dired-remove-all-spaces ()
  ;; Made with reference to `image-dired-line-up'.
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (while (not (eobp))
      (if (image-dired-image-at-point-p)
          (forward-char)
        (delete-char 1)))))

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

(defun xmp-image-dired-filter-required-properties ()
  "Return a list of expanded names of properties required for filtering."
  (mapcar #'car xmp-image-dired-filter-alist))

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

(defun xmp-image-dired-filter-set (prop-ename pred)
  (if pred
      (setf (xmp-xml-ename-alist-get prop-ename
                                     xmp-image-dired-filter-alist)
            pred)
    (setf (xmp-xml-ename-alist-get prop-ename
                                   xmp-image-dired-filter-alist nil t)
          nil)))

;;;###autoload
(defun xmp-image-dired-filter-property (prop-ename pred &optional arg)
  "Display files for which the value of the property specified by
PROP-ENAME satisfies the predicate PRED.

If the prefix argument is - or 0, remove the filter for the property. If
the prefix argument is any other non-nil value, invert the specified
condition."
  (interactive (xmp-filter-read-property-condition) image-dired-thumbnail-mode)
  (xmp-image-dired-filter-set prop-ename
                              (xmp-filter-gen-property-predicate pred arg))
  (xmp-image-dired-filter-thumbnails))

;;;###autoload
(defun xmp-image-dired-filter-rating (condition &optional arg)
  (interactive (xmp-filter-read-rating-condition) image-dired-thumbnail-mode)
  (xmp-image-dired-filter-set xmp-xmp:Rating
                              (xmp-filter-gen-rating-predicate condition arg))
  (xmp-image-dired-filter-thumbnails))

;;;###autoload
(defun xmp-image-dired-filter-label (label &optional arg)
  (interactive (xmp-filter-read-label-condition) image-dired-thumbnail-mode)
  (xmp-image-dired-filter-set xmp-xmp:Label
                              (xmp-filter-gen-label-predicate label arg))
  (xmp-image-dired-filter-thumbnails))

;;;###autoload
(defun xmp-image-dired-filter-subjects (subjects &optional arg)
  (interactive (xmp-filter-read-subjects-condition) image-dired-thumbnail-mode)
  (xmp-image-dired-filter-set xmp-dc:subject
                              (xmp-filter-gen-subjects-predicate subjects arg))
  (xmp-image-dired-filter-thumbnails))

;;;###autoload
(defun xmp-image-dired-filter-title (pred &optional arg)
  (interactive (xmp-filter-read-property-condition xmp-dc:title)
               image-dired-thumbnail-mode)
  (xmp-image-dired-filter-set xmp-dc:title
                              (xmp-filter-gen-property-predicate pred arg))
  (xmp-image-dired-filter-thumbnails))

;;;###autoload
(defun xmp-image-dired-filter-description (pred &optional arg)
  (interactive (xmp-filter-read-property-condition xmp-dc:description)
               image-dired-thumbnail-mode)
  (xmp-image-dired-filter-set xmp-dc:description
                              (xmp-filter-gen-property-predicate pred arg))
  (xmp-image-dired-filter-thumbnails))

;;;###autoload
(defun xmp-image-dired-filter-creators (pred &optional arg)
  (interactive (xmp-filter-read-property-condition xmp-dc:creator)
               image-dired-thumbnail-mode)
  (xmp-image-dired-filter-set xmp-dc:creator
                              (xmp-filter-gen-property-predicate pred arg))
  (xmp-image-dired-filter-thumbnails))

;;;; Sort

;;;###autoload
(defun xmp-image-dired-sort-by-property (prop-ename &optional reverse)
  "Sort thumbnails in image-dired buffer by the XMP property specified by
PROP-ENAME.
If the prefix argument or REVERSE is non-nil, sort in reverse order.
If PROP-ENAME is nil, call `xmp-image-dired-sort-by-file-name'."
  (interactive
   (list (xmp-read-property-ename (xmp-msg "Key property: "))
         current-prefix-arg)
   image-dired-thumbnail-mode)

  (if (null prop-ename)
      (xmp-image-dired-sort-by-file-name reverse)
    (let* ((sort-key (xmp-make-file-property-sort-key prop-ename reverse))
           (fun-key (xmp-file-property-sort-key-fun-key sort-key))
           (fun-less (xmp-file-property-sort-key-fun-less sort-key)))
      (xmp-image-dired-remove-all-spaces)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (sort-subr nil #'forward-char nil
                   (lambda ()
                     (funcall fun-key (image-dired-original-file-name)))
                   nil
                   fun-less))
      ;; Update spaces and line breaks
      (image-dired--line-up-with-method))))

;;;###autoload
(defun xmp-image-dired-sort-by-file-name (&optional reverse)
  "Sort thumbnails in image-dired buffer by filename.
If the prefix argument or REVERSE is non-nil, sort in reverse order."
  (interactive "P" image-dired-thumbnail-mode)
  (xmp-image-dired-remove-all-spaces)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (sort-subr reverse #'forward-char nil
               (lambda ()
                 (or (image-dired-original-file-name) ""))
               nil
               ;; Use string-collate-lessp? See `ls-lisp-string-lessp'
               #'string<))
  ;; Update spaces and line breaks
  (image-dired--line-up-with-method))

;;;###autoload
(defun xmp-image-dired-sort-by-rating (&optional reverse)
  (interactive "P")
  (xmp-image-dired-sort-by-property xmp-xmp:Rating reverse))

;;;###autoload
(defun xmp-image-dired-sort-by-label (&optional reverse)
  (interactive "P")
  (xmp-image-dired-sort-by-property xmp-xmp:Label reverse))

;;;###autoload
(defun xmp-image-dired-sort-by-subjects (&optional reverse)
  (interactive "P")
  (xmp-image-dired-sort-by-property xmp-dc:subject reverse))

;;;###autoload
(defun xmp-image-dired-sort-by-title (&optional reverse)
  (interactive "P")
  (xmp-image-dired-sort-by-property xmp-dc:title reverse))

;;;###autoload
(defun xmp-image-dired-sort-by-description (&optional reverse)
  (interactive "P")
  (xmp-image-dired-sort-by-property xmp-dc:description reverse))

;;;###autoload
(defun xmp-image-dired-sort-by-creators (&optional reverse)
  (interactive "P")
  (xmp-image-dired-sort-by-property xmp-dc:creator reverse))

;;;; Marked Files

(defun xmp-image-dired-get-marked-files ()
  (let (files)
    (image-dired--with-marked
     (push (image-dired-original-file-name) files))
    (unless files
      (user-error "No files specified"))
    (nreverse files)))

(provide 'xmp-image-dired)
;;; xmp-image-dired.el ends here
