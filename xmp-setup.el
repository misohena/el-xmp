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
;; - ' s r : Set rate
;; - ' s l : Set label
;; - ' s s : Set subjects
;; - ' a s : Add subjects
;; - ' r s : Remove subjects

;; - ' g r : Get rate
;; - ' g l : Get label
;; - ' g s : Get subjects
;; - ' g a : Get all properties

;; - ' m r : Mark by rate
;; - ' m l : Mark by label
;; - ' m s : Mark by subjects

;; - ' f r : Filter by rate
;; - ' f l : Filter by label
;; - ' f s : Filter by subjects
;; - ' f - : Clear filter

;;; Code:

;;;; Autoloads

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
(autoload 'xmp-show-file-title "xmp-commands" nil t)
(autoload 'xmp-set-file-title "xmp-commands" nil t)
(autoload 'xmp-show-file-description "xmp-commands" nil t)
(autoload 'xmp-set-file-description "xmp-commands" nil t)
(autoload 'xmp-show-file-creators "xmp-commands" nil t)
(autoload 'xmp-set-file-creators "xmp-commands" nil t)
(autoload 'xmp-show-file-properties "xmp-commands" nil t)

;; xmp-dired.el
(autoload 'xmp-dired-mark-rating "xmp-dired" nil t)
(autoload 'xmp-dired-mark-label "xmp-dired" nil t)
(autoload 'xmp-dired-mark-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-mark-title "xmp-dired" nil t)
(autoload 'xmp-dired-mark-description "xmp-dired" nil t)
(autoload 'xmp-dired-mark-creator "xmp-dired" nil t)
(autoload 'xmp-dired-do-rate "xmp-dired" nil t)
(autoload 'xmp-dired-do-set-label "xmp-dired" nil t)
(autoload 'xmp-dired-do-set-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-do-add-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-do-remove-subjects "xmp-dired" nil t)
(autoload 'xmp-dired-do-set-title "xmp-dired" nil t)
(autoload 'xmp-dired-do-set-description "xmp-dired" nil t)

;; xmp-image-dired.el
(autoload 'xmp-image-dired-filter-clear "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-filter-rating "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-filter-label "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-filter-subjects "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-do-rate "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-do-set-label "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-do-set-subjects "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-do-add-subjects "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-do-remove-subjects "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-do-set-title "xmp-image-dired" nil t)
(autoload 'xmp-image-dired-do-set-description "xmp-image-dired" nil t)

;;;; xmp-dired-mode

(defvar xmp-dired-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "'mr") 'xmp-dired-mark-rating)
    (define-key km (kbd "'ml") 'xmp-dired-mark-label)
    (define-key km (kbd "'ms") 'xmp-dired-mark-subjects)
    (define-key km (kbd "'mt") 'xmp-dired-mark-title)
    (define-key km (kbd "'md") 'xmp-dired-mark-description)
    (define-key km (kbd "'mc") 'xmp-dired-mark-creator)
    (define-key km (kbd "'sr") 'xmp-dired-do-rate)
    (define-key km (kbd "'sl") 'xmp-dired-do-set-label)
    (define-key km (kbd "'ss") 'xmp-dired-do-set-subjects)
    (define-key km (kbd "'as") 'xmp-dired-do-add-subjects)
    (define-key km (kbd "'rs") 'xmp-dired-do-remove-subjects)
    (define-key km (kbd "'st") 'xmp-dired-do-set-title)
    (define-key km (kbd "'sd") 'xmp-dired-do-set-description)
    (define-key km (kbd "'ga") 'xmp-show-file-properties)
    (define-key km (kbd "'gr") 'xmp-show-file-rating)
    (define-key km (kbd "'gl") 'xmp-show-file-label)
    (define-key km (kbd "'gs") 'xmp-show-file-subjects)
    (define-key km (kbd "'gt") 'xmp-show-file-title)
    (define-key km (kbd "'gd") 'xmp-show-file-description)
    (define-key km (kbd "'gc") 'xmp-show-file-creators)
    km))

(define-minor-mode xmp-dired-mode
  "")

;;;; xmp-image-dired-thumbnail-mode

(defvar xmp-image-dired-thumbnail-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "'f-") 'xmp-image-dired-filter-clear)
    (define-key km (kbd "'fr") 'xmp-image-dired-filter-rating)
    (define-key km (kbd "'fl") 'xmp-image-dired-filter-label)
    (define-key km (kbd "'fs") 'xmp-image-dired-filter-subjects)
    (define-key km (kbd "'sr") 'xmp-image-dired-do-rate)
    (define-key km (kbd "'sl") 'xmp-image-dired-do-set-label)
    (define-key km (kbd "'ss") 'xmp-image-dired-do-set-subjects)
    (define-key km (kbd "'as") 'xmp-image-dired-do-add-subjects)
    (define-key km (kbd "'rs") 'xmp-image-dired-do-remove-subjects)
    (define-key km (kbd "'st") 'xmp-image-dired-do-set-title)
    (define-key km (kbd "'sd") 'xmp-image-dired-do-set-description)
    (define-key km (kbd "'ga") 'xmp-show-file-properties)
    (define-key km (kbd "'gr") 'xmp-show-file-rating)
    (define-key km (kbd "'gl") 'xmp-show-file-label)
    (define-key km (kbd "'gs") 'xmp-show-file-subjects)
    (define-key km (kbd "'gt") 'xmp-show-file-title)
    (define-key km (kbd "'gd") 'xmp-show-file-description)
    (define-key km (kbd "'gc") 'xmp-show-file-creators)
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
