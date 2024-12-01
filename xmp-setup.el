;;; xmp-setup.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

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

;; This file provides an example of el-xmp configuration.
;; Call `xmp-setup-default'

;; Assign keys to manipulate XMP properties in dired and image-dired.
;; - ' s r : Set rating
;; - ' s l : Set label
;; - ' s s : Set subjects
;; - ' a s : Add subjects
;; - ' r s : Remove subjects
;; - ' s t : Set title
;; - ' s d : Set description
;; - ' s c : Set creators

;; - ' g r : Get rating
;; - ' g l : Get label
;; - ' g s : Get subjects
;; - ' g t : Get title
;; - ' g d : Get description
;; - ' g c : Get creators

;; - ' g p : Get properties
;; - ' g a : Get all properties

;; - ' e p : Edit properties
;; - ' e a : Edit all properties

;; - ' m r : Mark by rating
;; - ' m l : Mark by label
;; - ' m s : Mark by subjects
;; - ' m t : Mark by title
;; - ' m d : Mark by description
;; - ' m c : Mark by creators

;; - ' f p : Filter by property
;; - ' f - : Clear filter
;; - ' f r : Filter by rating
;; - ' f l : Filter by label
;; - ' f s : Filter by subjects
;; - ' f t : Filter by title
;; - ' f d : Filter by description
;; - ' f c : Filter by creators

;; - ' S p : Sort by property
;; - ' S - : Clear sort
;; - ' S r : Sort by rating
;; - ' S l : Sort by label
;; - ' S s : Sort by subjects
;; - ' S t : Sort by title
;; - ' S d : Sort by description
;; - ' S c : Sort by creators

;; - ' C p : Toggle property column
;; - ' C r : Toggle rating column
;; - ' C l : Toggle label column
;; - ' C s : Toggle subjects column
;; - ' C t : Toggle title column
;; - ' C d : Toggle description column
;; - ' C c : Toggle creators column
;; - ' C - : Remove all columns

;; - ' l m : List managed files
;; - ' l S : List stray metadata

;;; Code:

;;;; Autoloads

;; xmp.el
(autoload 'xmp-clear-file-cache "xmp" nil t)
(autoload 'xmp-clear-file-cache-in-dir "xmp" nil t)
(autoload 'xmp-clear-file-cache-under-dir "xmp" nil t)
(autoload 'xmp-clear-invalid-file-cache "xmp" nil t)
(autoload 'xmp-clear-invalid-file-cache-in-dir "xmp" nil t)
(autoload 'xmp-clear-invalid-file-cache-under-dir "xmp" nil t)
(autoload 'xmp-file-cache-memory-clear "xmp" nil t)
(autoload 'xmp-remove-external-file-metadata "xmp" nil t)
(autoload 'xmp-remove-external-file-metadata-in-dir "xmp" nil t)
(autoload 'xmp-move-external-file-metadata "xmp" nil t)
(autoload 'xmp-move-external-file-metadata-in-dir "xmp" nil t)

;; xmp-commands.el
(autoload 'xmp-show-file-rating "xmp-commands" nil t)
(autoload 'xmp-rate-file "xmp-commands" nil t)
(autoload 'xmp-rate-file-1 "xmp-commands" nil t)
(autoload 'xmp-rate-file-2 "xmp-commands" nil t)
(autoload 'xmp-rate-file-3 "xmp-commands" nil t)
(autoload 'xmp-rate-file-4 "xmp-commands" nil t)
(autoload 'xmp-rate-file-5 "xmp-commands" nil t)
(autoload 'xmp-rate-file-0 "xmp-commands" nil t)
(autoload 'xmp-rate-file--1 "xmp-commands" nil t)
(autoload 'xmp-show-file-label "xmp-commands" nil t)
(autoload 'xmp-set-file-label "xmp-commands" nil t)
(autoload 'xmp-show-file-subjects "xmp-commands" nil t)
(autoload 'xmp-set-file-subjects "xmp-commands" nil t)
(autoload 'xmp-add-file-subjects "xmp-commands" nil t)
(autoload 'xmp-remove-file-subjects "xmp-commands" nil t)
(autoload 'xmp-show-file-title "xmp-commands" nil t)
(autoload 'xmp-set-file-title "xmp-commands" nil t)
(autoload 'xmp-show-file-description "xmp-commands" nil t)
(autoload 'xmp-set-file-description "xmp-commands" nil t)
(autoload 'xmp-show-file-creators "xmp-commands" nil t)
(autoload 'xmp-set-file-creators "xmp-commands" nil t)
(autoload 'xmp-show-file-properties "xmp-commands" nil t)
(autoload 'xmp-show-file-properties-all "xmp-commands" nil t)
(autoload 'xmp-edit-file-properties "xmp-commands" nil t)
(autoload 'xmp-edit-file-properties-all "xmp-commands" nil t)
(autoload 'xmp-move-file-properties-from-db-to-sidecar "xmp-commands" nil t)
(autoload 'xmp-move-dir-file-properties-from-db-to-sidecar "xmp-commands" nil t)
(autoload 'xmp-move-file-properties-from-sidecar-to-db "xmp-commands" nil t)
(autoload 'xmp-move-dir-file-properties-from-sidecar-to-db "xmp-commands" nil t)
(autoload 'xmp-list-managed-files-in-dir "xmp-commands" nil t)
(autoload 'xmp-list-stray-file-metadata-in-db "xmp-commands" nil t)
(autoload 'xmp-relocate-stray-file-metadata-in-dir "xmp-commands" nil t)

;; xmp-dired.el
(autoload 'xmp-dired-mark-rating "xmp-dired" nil t)
(autoload 'xmp-dired-mark-label "xmp-dired" nil t)
(autoload 'xmp-dired-mark-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-mark-title "xmp-dired" nil t)
(autoload 'xmp-dired-mark-description "xmp-dired" nil t)
(autoload 'xmp-dired-mark-creator "xmp-dired" nil t)
(autoload 'xmp-dired-mark-stray-sidecar-files "xmp-dired" nil t)
(autoload 'xmp-dired-do-rate "xmp-dired" nil t)
(autoload 'xmp-dired-do-set-label "xmp-dired" nil t)
(autoload 'xmp-dired-do-set-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-do-add-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-do-remove-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-do-set-title "xmp-dired" nil t)
(autoload 'xmp-dired-do-set-description "xmp-dired" nil t)
(autoload 'xmp-dired-do-set-creators "xmp-dired" nil t)
(autoload 'xmp-dired-do-edit-properties "xmp-dired" nil t)
(autoload 'xmp-dired-do-edit-properties-all "xmp-dired" nil t)
(autoload 'xmp-dired-filter-property "xmp-dired" nil t)
(autoload 'xmp-dired-filter-clear "xmp-dired" nil t)
(autoload 'xmp-dired-filter-rating "xmp-dired" nil t)
(autoload 'xmp-dired-filter-label "xmp-dired" nil t)
(autoload 'xmp-dired-filter-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-filter-title "xmp-dired" nil t)
(autoload 'xmp-dired-filter-description "xmp-dired" nil t)
(autoload 'xmp-dired-filter-creators "xmp-dired" nil t)
(autoload 'xmp-dired-filter-toggle-sidecar "xmp-dired" nil t)
(autoload 'xmp-dired-filter-show-sidecar "xmp-dired" nil t)
(autoload 'xmp-dired-filter-hide-sidecar "xmp-dired" nil t)
(autoload 'xmp-dired-sort-by-property "xmp-dired" nil t)
(autoload 'xmp-dired-sort-clear "xmp-dired" nil t)
(autoload 'xmp-dired-sort-by-rating "xmp-dired" nil t)
(autoload 'xmp-dired-sort-by-label "xmp-dired" nil t)
(autoload 'xmp-dired-sort-by-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-sort-by-title "xmp-dired" nil t)
(autoload 'xmp-dired-sort-by-description "xmp-dired" nil t)
(autoload 'xmp-dired-sort-by-creators "xmp-dired" nil t)
(autoload 'xmp-dired-add-column "xmp-dired" nil t)
(autoload 'xmp-dired-remove-column "xmp-dired" nil t)
(autoload 'xmp-dired-toggle-column "xmp-dired" nil t)
(autoload 'xmp-dired-toggle-column-rating "xmp-dired" nil t)
(autoload 'xmp-dired-toggle-column-label "xmp-dired" nil t)
(autoload 'xmp-dired-toggle-column-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-toggle-column-title "xmp-dired" nil t)
(autoload 'xmp-dired-toggle-column-description "xmp-dired" nil t)
(autoload 'xmp-dired-toggle-column-creators "xmp-dired" nil t)
(autoload 'xmp-dired-remove-all-columns "xmp-dired" nil t)

;; xmp-image-dired.el
(autoload 'xmp-image-dired-filter-property "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-filter-clear "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-filter-rating "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-filter-label "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-filter-subjects "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-filter-title "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-filter-description "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-filter-creators "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-sort-by-property "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-sort-by-file-name "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-sort-by-rating "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-sort-by-label "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-sort-by-subjects "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-sort-by-title "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-sort-by-description "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-sort-by-creators "xmp-image-dired" nil t)

;;;; xmp-dired-mode

(defvar xmp-dired-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "'mr") 'xmp-dired-mark-rating)
    (define-key km (kbd "'ml") 'xmp-dired-mark-label)
    (define-key km (kbd "'ms") 'xmp-dired-mark-subjects)
    (define-key km (kbd "'mt") 'xmp-dired-mark-title)
    (define-key km (kbd "'md") 'xmp-dired-mark-description)
    (define-key km (kbd "'mc") 'xmp-dired-mark-creator)
    (define-key km (kbd "'mS") 'xmp-dired-mark-stray-sidecar-files)
    (define-key km (kbd "'sr") 'xmp-dired-do-rate)
    (define-key km (kbd "'sl") 'xmp-dired-do-set-label)
    (define-key km (kbd "'ss") 'xmp-dired-do-set-subjects)
    (define-key km (kbd "'as") 'xmp-dired-do-add-subjects)
    (define-key km (kbd "'rs") 'xmp-dired-do-remove-subjects)
    (define-key km (kbd "'st") 'xmp-dired-do-set-title)
    (define-key km (kbd "'sd") 'xmp-dired-do-set-description)
    (define-key km (kbd "'sc") 'xmp-dired-do-set-creators)
    (define-key km (kbd "'gr") 'xmp-show-file-rating)
    (define-key km (kbd "'gl") 'xmp-show-file-label)
    (define-key km (kbd "'gs") 'xmp-show-file-subjects)
    (define-key km (kbd "'gt") 'xmp-show-file-title)
    (define-key km (kbd "'gd") 'xmp-show-file-description)
    (define-key km (kbd "'gc") 'xmp-show-file-creators)
    (define-key km (kbd "'gp") 'xmp-show-file-properties)
    (define-key km (kbd "'ga") 'xmp-show-file-properties-all)
    (define-key km (kbd "'ep") 'xmp-dired-do-edit-properties)
    (define-key km (kbd "'ea") 'xmp-dired-do-edit-properties-all)
    (define-key km (kbd "'lm") 'xmp-list-managed-files-in-dir)
    (define-key km (kbd "'lS") 'xmp-list-stray-file-metadata-in-db)
    (define-key km (kbd "'RS") 'xmp-relocate-stray-file-metadata-in-dir)
    (define-key km (kbd "'fp") 'xmp-dired-filter-property)
    (define-key km (kbd "'f-") 'xmp-dired-filter-clear)
    (define-key km (kbd "'fr") 'xmp-dired-filter-rating)
    (define-key km (kbd "'fl") 'xmp-dired-filter-label)
    (define-key km (kbd "'fs") 'xmp-dired-filter-subjects)
    (define-key km (kbd "'ft") 'xmp-dired-filter-title)
    (define-key km (kbd "'fd") 'xmp-dired-filter-description)
    (define-key km (kbd "'fc") 'xmp-dired-filter-creators)
    (define-key km (kbd "'Sp") 'xmp-dired-sort-by-property)
    (define-key km (kbd "'S-") 'xmp-dired-sort-clear)
    (define-key km (kbd "'Sr") 'xmp-dired-sort-by-rating)
    (define-key km (kbd "'Sl") 'xmp-dired-sort-by-label)
    (define-key km (kbd "'Ss") 'xmp-dired-sort-by-subjects)
    (define-key km (kbd "'St") 'xmp-dired-sort-by-title)
    (define-key km (kbd "'Sd") 'xmp-dired-sort-by-description)
    (define-key km (kbd "'Sc") 'xmp-dired-sort-by-creators)
    (define-key km (kbd "'Cp") 'xmp-dired-toggle-column)
    (define-key km (kbd "'C-") 'xmp-dired-remove-all-columns)
    (define-key km (kbd "'Cr") 'xmp-dired-toggle-column-rating)
    (define-key km (kbd "'Cl") 'xmp-dired-toggle-column-label)
    (define-key km (kbd "'Cs") 'xmp-dired-toggle-column-subjects)
    (define-key km (kbd "'Ct") 'xmp-dired-toggle-column-title)
    (define-key km (kbd "'Cd") 'xmp-dired-toggle-column-description)
    (define-key km (kbd "'Cc") 'xmp-dired-toggle-column-creators)
    km))

(define-minor-mode xmp-dired-mode
  "")

;;;; xmp-image-dired-thumbnail-mode

(defvar xmp-image-dired-thumbnail-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "'sr") 'xmp-rate-file)
    (define-key km (kbd "'sl") 'xmp-set-file-label)
    (define-key km (kbd "'ss") 'xmp-set-file-subjects)
    (define-key km (kbd "'as") 'xmp-add-file-subjects)
    (define-key km (kbd "'rs") 'xmp-remove-file-subjects)
    (define-key km (kbd "'st") 'xmp-set-file-title)
    (define-key km (kbd "'sd") 'xmp-set-file-description)
    (define-key km (kbd "'sc") 'xmp-set-file-creators)
    (define-key km (kbd "'gr") 'xmp-show-file-rating)
    (define-key km (kbd "'gl") 'xmp-show-file-label)
    (define-key km (kbd "'gs") 'xmp-show-file-subjects)
    (define-key km (kbd "'gt") 'xmp-show-file-title)
    (define-key km (kbd "'gd") 'xmp-show-file-description)
    (define-key km (kbd "'gc") 'xmp-show-file-creators)
    (define-key km (kbd "'gp") 'xmp-show-file-properties)
    (define-key km (kbd "'ga") 'xmp-show-file-properties-all)
    (define-key km (kbd "'ep") 'xmp-edit-file-properties)
    (define-key km (kbd "'ea") 'xmp-edit-file-properties-all)
    (define-key km (kbd "'lm") 'xmp-list-managed-files-in-dir)
    (define-key km (kbd "'lS") 'xmp-list-stray-file-metadata-in-db)
    (define-key km (kbd "'RS") 'xmp-relocate-stray-file-metadata-in-dir)
    (define-key km (kbd "'fp") 'xmp-image-dired-filter-property)
    (define-key km (kbd "'f-") 'xmp-image-dired-filter-clear)
    (define-key km (kbd "'fr") 'xmp-image-dired-filter-rating)
    (define-key km (kbd "'fl") 'xmp-image-dired-filter-label)
    (define-key km (kbd "'fs") 'xmp-image-dired-filter-subjects)
    (define-key km (kbd "'ft") 'xmp-image-dired-filter-title)
    (define-key km (kbd "'fd") 'xmp-image-dired-filter-description)
    (define-key km (kbd "'fc") 'xmp-image-dired-filter-creators)
    (define-key km (kbd "'Sp") 'xmp-image-dired-sort-by-property)
    (define-key km (kbd "'S-") 'xmp-image-dired-sort-by-file-name)
    (define-key km (kbd "'Sr") 'xmp-image-dired-sort-by-rating)
    (define-key km (kbd "'Sl") 'xmp-image-dired-sort-by-label)
    (define-key km (kbd "'Ss") 'xmp-image-dired-sort-by-subjects)
    (define-key km (kbd "'St") 'xmp-image-dired-sort-by-title)
    (define-key km (kbd "'Sd") 'xmp-image-dired-sort-by-description)
    (define-key km (kbd "'Sc") 'xmp-image-dired-sort-by-creators)
    km))

(define-minor-mode xmp-image-dired-thumbnail-mode
  "")

;;;; Setup default

(define-minor-mode xmp-global-default-mode
  ""
  :group 'files
  :global t
  (if xmp-global-default-mode
      (progn
        (add-hook 'dired-mode-hook 'xmp-dired-mode)
        (add-hook 'image-dired-thumbnail-mode-hook 'xmp-image-dired-thumbnail-mode))
    (remove-hook 'dired-mode-hook 'xmp-dired-mode)
    (remove-hook 'image-dired-thumbnail-mode-hook 'xmp-image-dired-thumbnail-mode)))

;;;###autoload
(defun xmp-setup-default ()
  (xmp-global-default-mode))

(provide 'xmp-setup)
;;; xmp-setup.el ends here
