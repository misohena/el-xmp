;;; xmp-editor.el --- XMP metadata editor           -*- lexical-binding: t; -*-

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

;; XMP metadata editor based on Widget Library.

;; Example usage:
;;   (xmp-editor-open-files (directory-files "." t "\\.jpg$"))
;;
;;   (xmp-editor-open-files (dired-get-marked-files) (list xmp-dc:title))

;;; Code:

(require 'xmp)
(require 'wid-edit)
(require 'text-property-search)

;;;; Widgets
;;;;; XMP Property

;; Example:
;; (progn
;;   (pop-to-buffer (generate-new-buffer "*Widget Example*"))
;;   (setq test-wid
;;         (widget-create 'xmp-property
;;                        :tag "Title"
;;                        :type '(text :size 20 :format " %v")
;;                        ;; :type '(xmp-lang-alt)
;;                          ;;:value '()
;;                          ;;:value '(("x-default" . "Hoge"))
;;                          ;;:value '(("ja" . "Hoge"))
;;                          ;;'(("x-default" . "Hoge"))
;;                          ;;'(("x-default" . "Hoge") ("ja" . "Hoge"))
;;                          ;;:value '(("ja" . "Hoge"))
;;                          ))
;;   (use-local-map widget-keymap)
;;   (widget-setup))

(define-widget 'xmp-property 'default
  "XMP Property in xmp-editor."
  :convert-widget 'widget-value-convert-widget
  :format " %t:%v"
  :value-create 'xmp-widget-property-value-create
  :value-get 'widget-child-value-get)

(defun xmp-widget-property-value-create (widget)
  (let ((child (widget-create-child-and-convert
                widget (widget-get widget :type)
                :notify 'xmp-widget-property-notify
                (widget-get widget :value))))
    (widget-put widget :children (list child))
    (widget-put widget :xmp-init-value (widget-value child))
    (widget-put widget :xmp-modified-p nil)))

(defun xmp-widget-property-tag-begin (widget)
  (1+ (widget-get widget :from)))

(defun xmp-widget-property-tag-end (widget)
  ;; label:| [   ]
  (1- (widget-get (car (widget-get widget :children)) :from)))

(defun xmp-widget-property-notify (child &rest _)
  ;;(message "Enter xmp-widget-property-notify")
  (let ((parent (widget-get child :parent)))
    (xmp-widget-property-update-modified
     parent
     (not (equal
           (widget-value child)
           (widget-get parent :xmp-init-value))))))

(defun xmp-widget-property-update-modified (widget modified)
  ;; (message "Enter xmp-widget-property-update-modified modified=%s" modified)
  (unless (eq modified (widget-get widget :xmp-modified-p))
    (widget-put widget :xmp-modified-p modified)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      ;; *Label: [    ]
      (let ((from (widget-get widget :from)))
        (save-excursion
          (goto-char (1+ from))
          (insert (if modified "*" " "))
          (goto-char from)
          (delete-char 1)))
      ;; ;; Label* [    ]
      ;; (let ((from (1- (widget-get (car (widget-get widget :children))
      ;;                             :from))))
      ;;   (save-excursion
      ;;     (goto-char from)
      ;;     (insert (if modified "*" ":"))
      ;;     (delete-char 1)))
      )))


(defun xmp-widget-property-modified-p (widget)
  (widget-get widget :xmp-modified-p))

(defun xmp-widget-property-clear-modified-p (widget)
  (when (widget-get widget :xmp-modified-p)
    (widget-put widget :xmp-init-value (widget-child-value-get widget))
    (xmp-widget-property-update-modified widget nil)))


;;;;; LangAlt

;; Example:
;; (progn
;;   (pop-to-buffer (generate-new-buffer "*Widget Example*"))
;;   (setq test-wid
;;   (widget-create 'xmp-lang-alt
;;                  :tag "Title"
;;                  ;;:value '()
;;                  ;;:value '(("x-default" . "Hoge"))
;;                  ;;:value '(("ja" . "Hoge"))
;;                  ;;'(("x-default" . "Hoge"))
;;                  ;;'(("x-default" . "Hoge") ("ja" . "Hoge"))
;;                  ))
;;   (use-local-map widget-keymap)
;;   (widget-setup))

(define-widget 'xmp-lang-alt 'default
  "XMP Language Alternative."
  :convert-widget 'widget-value-convert-widget
  :format "%v"
  :tag ""
  :value-create 'xmp-widget-lang-alt-value-create
  :value-get 'xmp-widget-lang-alt-value-get
  :validate 'widget-child-validate)

(defun xmp-widget-lang-alt-value-single-p (value)
  (or (null value)
      (and (null (cdr value))
           (equal (caar value) "x-default"))))

(defun xmp-widget-lang-alt-value-create (widget)
  (let* ((value (widget-get widget :value))
         (single-p (xmp-widget-lang-alt-value-single-p value)))
    (widget-put widget :xmp-single-p single-p)
    (widget-put widget :children
                (if single-p
                    (xmp-widget-lang-alt-single-value-create widget)
                  (xmp-widget-lang-alt-multiple-value-create widget)))))

(defun xmp-widget-lang-alt-value-get (widget)
  (if (widget-get widget :xmp-single-p)
      (xmp-widget-lang-alt-single-get-external-value widget)
    (xmp-widget-lang-alt-multiple-get-external-value widget)))

;; Single Item (x-default only)

(defun xmp-widget-lang-alt-single-value-create (widget)
  (list (widget-create-child-and-convert
         widget 'text
         :format " %v " ;; Do not specify "%v " because :from marker type is t
         :size 20
         :value (xmp-widget-lang-alt-single-to-internal
                 (widget-get widget :value)))

        (widget-create-child-and-convert
         widget 'insert-button
         :action 'xmp-widget-lang-alt-single-insert)
        ))

(defun xmp-widget-lang-alt-single-to-internal (alist)
  (cdar alist))

(defun xmp-widget-lang-alt-single-get-external-value (widget)
  (let ((text (widget-value (car (widget-get widget :children)))))
    (if (string-empty-p text)
        nil
      (list (cons "x-default" text)))))

(defun xmp-widget-lang-alt-single-insert (widget &rest _)
  (let* ((parent (widget-get widget :parent))
         (value (widget-value parent))
         (new-value
          (if (null value)
              (list (cons "" "") (cons "" ""))
            (append value (list (cons "" ""))))))
    (widget-value-set parent new-value)
    (widget-setup)))

;; Multiple Items

(defun xmp-widget-lang-alt-multiple-value-create (widget)
  (list (widget-create-child-and-convert
         widget
         '(alist
           :format "\n%v"
           :key-type
           (string :tag "Lang" :size 10
                   :format "%t:%v")
           :value-type
           (string :tag "" :format " %v")
           :notify xmp-widget-lang-alt-multiple-notify)
         :value (xmp-widget-lang-alt-multiple-to-internal
                 (widget-get widget :value)))))

(defun xmp-widget-lang-alt-multiple-to-internal (alist)
  (if (equal (caar alist) "x-default")
      (if (and (cdr alist)
               (not (string-empty-p (caadr alist)))
               (equal (cdar alist) (cdadr alist)))
          (cdr alist)
        (cons (cons "" (cdar alist)) (cdr alist)))
    alist))

(defun xmp-widget-lang-alt-multiple-get-external-value (widget)
  (when-let ((alist (widget-value (car (widget-get widget :children)))))
    (if-let ((result-alist (seq-remove (lambda (item)
                                         (or (string-empty-p (car item))
                                             (string-empty-p (cdr item))))
                                       alist)))
        (cons (cons "x-default" (cdar result-alist))
              result-alist)
      (if-let ((item
                (seq-find (lambda (item) (not (string-empty-p (cdr item))))
                          alist)))
          (list (cons "x-default" (cdr item)))
        nil))))

(defun xmp-widget-lang-alt-multiple-notify (widget _child &optional event)
  (let ((parent (widget-get widget :parent))
        (alist (widget-value widget)))
    ;; Change to single type
    (when (= (length alist) 1)
      (widget-value-set parent (list (cons "x-default" (cdar alist))))
      (widget-setup))

    ;; Notify parent
    (widget-apply parent :notify widget event)))

;;;;; Comma Separated Text (subject)

;; Example:
;; (progn
;;   (pop-to-buffer (generate-new-buffer "*Widget Example*"))
;;   (setq test-wid
;;         (widget-create 'xmp-comma-separated-text
;;                        '("Plant" "Flower")))
;;   (use-local-map widget-keymap)
;;   (widget-setup))

(define-widget 'xmp-comma-separated-text 'editable-field
  ""
  :tag ""
  :format " %v"
  ;; NOTE: Must be explicitly specified.
  ;; In cus-edit.el, the keymap of editable-field is forcibly
  ;; rewritten to `custom-field-keymap', so if you do not specify
  ;; anything, the keymap for the customization buffer will be used.
  :keymap widget-field-keymap
  :value-to-internal 'xmp-widget-comma-separated-text-value-to-internal
  :value-to-external 'xmp-widget-comma-separated-text-value-to-external)

(defun xmp-widget-comma-separated-text-value-to-internal (_widget value)
  (mapconcat
   (lambda (s)
     (replace-regexp-in-string "," "/" s t t))
   value
   ","))

(defun xmp-widget-comma-separated-text-value-to-external (_widget value)
  (split-string value "," t " +"))

;;;;; TODO: Label widget

;;;;; TODO: Rating widget


;;;; Target Property Names

(defcustom xmp-editor-target-properties
  '(("http://purl.org/dc/elements/1.1/" "title")
    ;;("http://purl.org/dc/elements/1.1/" "creator")
    ("http://purl.org/dc/elements/1.1/" "subject")
    ("http://purl.org/dc/elements/1.1/" "description")
    ;;("http://purl.org/dc/elements/1.1/" "date")
    ;;("http://ns.adobe.com/xap/1.0/" "CreateDate")
    ("http://ns.adobe.com/xap/1.0/" "Label")
    ("http://ns.adobe.com/xap/1.0/" "Rating"))
  "A list that specifies which properties to display in
`xmp-editor'."
  :type '(repeat
          (list
           (string :tag "Namespace name (URI)")
           (string :tag "Property local name")))
  :group 'xmp)

(defun xmp-editor-target-properties-enames ()
  (if xmp-editor-target-properties
      (cl-loop for (ns-name prop-local-name)
               in xmp-editor-target-properties
               collect (xmp-xml-ename (xmp-xml-ns-name ns-name)
                                      prop-local-name))
    nil))

;;;; Image File Regexp Cache

(defvar xmp-editor-image-file-regexp-cache nil)

(defun xmp-editor-create-image-file-regexp ()
  ;; Very slow
  (image-file-name-regexp))

(defun xmp-editor-image-file-p (file)
  (when xmp-editor-image-file-regexp-cache
    (string-match-p xmp-editor-image-file-regexp-cache file)))

;;;; XMP Editor Mode

;; NOTE: local-map doesn't work on editable fields.(See: `widget-specify-field')
;;       Use minor-mode map.

(defvar xmp-editor-minor-mode-map
  (let ((km (make-sparse-keymap)))
    ;;(set-keymap-parent km widget-keymap)
    ;; (define-key km (kbd "C-c C-c") 'xmp-editor-save)
    ;; (define-key km (kbd "C-x C-s") 'xmp-editor-save)
    (define-key km [remap save-buffer] 'xmp-editor-save)
    (define-key km (kbd "C-c C-n") 'xmp-editor-next-same-property)
    (define-key km (kbd "C-c C-p") 'xmp-editor-previous-same-property)
    (define-key km (kbd "C-c C-o") 'xmp-editor-open-target-file-at)
    km))

(define-minor-mode xmp-editor-minor-mode
  ""
  )

(defvar xmp-editor-mode-map widget-keymap)

(defvar-local xmp-editor-files nil)

(define-derived-mode xmp-editor-mode nil "XMPEdit"
  ""
  (xmp-editor-minor-mode)
  )

;;;; Initialization

(defconst xmp-editor-default-buffer-name "*XMP Files Editor*")

;;;###autoload
(defun xmp-editor-open-files (files &optional
                                    prop-ename-list
                                    buffer)
  ;; TODO: Warn if there is an existing editor
  (xmp-editor-create-files-buffer (or buffer xmp-editor-default-buffer-name)
                                  (seq-remove #'xmp-sidecar-file-p files)
                                  (or prop-ename-list
                                      (xmp-editor-target-properties-enames))))

(defun xmp-editor-create-files-buffer (buffer files prop-ename-list)
  (let ((dir default-directory))
    (pop-to-buffer-same-window buffer)
    (xmp-editor-initialize-files-buffer files prop-ename-list dir)))

(defun xmp-editor-initialize-files-buffer (files prop-ename-list &optional dir)
  ;; Activating major mode does following:
  ;; - Call (kill-all-local-variables)
  ;; - Call (use-local-map xmp-editor-mode-map)
  (xmp-editor-mode)
  (erase-buffer)
  (remove-overlays)

  (when dir
    (setq default-directory dir))

  (xmp-editor-insert-files-header)

  (let ((xmp-editor-image-file-regexp-cache
         (xmp-editor-create-image-file-regexp)))

    (setq-local xmp-editor-files
                (cl-loop for file in files
                         ;;unless (xmp-sidecar-file-p file)
                         collect
                         (prog1 (xmp-editor-insert-file file prop-ename-list)
                           (insert "\n")))))

  (xmp-editor-insert-files-footer)

  (widget-setup)
  (widget-forward 1)
  (set-buffer-modified-p nil))

(defun xmp-editor-insert-files-header ()
  (insert
   (substitute-command-keys
    (concat
     "\\[xmp-editor-save]: " (xmp-msg "Save Properties") "\n"
     "\\[xmp-editor-next-same-property] / \\[xmp-editor-previous-same-property]: "
     (xmp-msg "Next/Previous Image") "\n"
     "\\[widget-forward] / \\[widget-backward]: "
     (xmp-msg "Next/Previous Widget") "\n"
     "\\[xmp-editor-open-target-file-at]: " (xmp-msg "Open File") "\n"
     ))
   "\n"))

(defun xmp-editor-insert-files-footer ()
  )

(defun xmp-editor-insert-file (file prop-ename-list)
  (setq file (expand-file-name file))
  (xmp-editor-insert-file-heading file)
  (when (xmp-editor-image-file-p file)
    (xmp-editor-insert-file-thumbnail file))
  (xmp-editor-file-info
   file
   (xmp-editor-insert-file-properties file prop-ename-list)))

(defun xmp-editor-insert-file-heading (file)
  (xmp-editor-insert-heading (format (xmp-msg "File: %s\n") file)
                             1
                             (list 'xmp-heading-file file)))

(defun xmp-editor-insert-heading (heading-string heading-level text-props)
  (insert (apply #'propertize
                 heading-string
                 'xmp-heading t
                 'xmp-heading-level heading-level
                 text-props)))

(defun xmp-editor-insert-file-thumbnail (file)
  (require 'image-dired)
  (when (fboundp 'image-dired-thumb-name)
    (insert-image (create-image (image-dired-thumb-name file)))
    (insert "\n")))

(defun xmp-editor-insert-file-properties (file prop-ename-list)
  (let* ((ns-name-prefix-alist (xmp-xml-standard-ns-name-prefix-alist))
         (props (xmp-enumerate-file-properties file
                                               prop-ename-list
                                               ns-name-prefix-alist)))
    ;; Return property widget alist
    (xmp-editor-insert-properties props
                                  prop-ename-list
                                  ns-name-prefix-alist)))

(defun xmp-editor-insert-properties (props
                                     prop-ename-list
                                     ns-name-prefix-alist)
  (let (max-width labels)
    (cl-loop for prop-ename in prop-ename-list
             for label = (xmp-editor-property-label prop-ename
                                                    ns-name-prefix-alist)
             maximize (string-width label) into v-max-width
             collect label into v-labels
             finally do (setq max-width v-max-width labels v-labels))

    (cl-loop for prop-ename in prop-ename-list
             for label in labels
             for aligned-label = (concat
                                  (make-string (max 0 (- max-width
                                                         (string-width label)))
                                               ? )
                                  label)
             collect (cons
                      prop-ename
                      (xmp-editor-insert-property prop-ename
                                                  props
                                                  aligned-label)))))

(defun xmp-editor-property-label (prop-ename ns-name-prefix-alist)
  ;; TODO: Generate more user-friendly text. Add xmp-editor-property-label-alist?
  ;; TODO: Move to xmp-commands.el or xmp.el?
  (concat
   (or
    (xmp-default-namespace-prefix (xmp-xml-ename-ns prop-ename))
    (xmp-xml-ns-name-to-prefix (xmp-xml-ename-ns prop-ename)
                               ns-name-prefix-alist
                               t)
    ;; (xmp-xml-ns-name-string (xmp-xml-ename-ns prop-ename)) ;;Too long?
    "")
   ":"
   (xmp-xml-ename-local prop-ename)))

(defun xmp-editor-insert-property (prop-ename
                                   props
                                   ns-name-prefix-alist-or-label)
  (let* ((label (if (stringp ns-name-prefix-alist-or-label)
                    ns-name-prefix-alist-or-label
                  (xmp-editor-property-label prop-ename
                                             ns-name-prefix-alist-or-label)))
         (pvalue (xmp-xml-ename-alist-get prop-ename props))
         (widget
          (cond
           ((xmp-xml-ename-equal prop-ename xmp-dc:subject)
            (widget-create
             'xmp-property
             :tag label
             :type '(xmp-comma-separated-text :format " %v")
             :value-to-internal (lambda (_widget pvalue)
                                  (xmp-pvalue-as-text-list pvalue))
             :value-to-external (lambda (_widget value)
                                  (xmp-pvalue-make-bag-from-text-list
                                   value))
             pvalue))
           (t
            (pcase (xmp-defined-property-type prop-ename)
              ((or 'Text 'Real 'Integer 'URI 'MIMEType 'AgentName 'Date 'GUID)
               (widget-create
                'xmp-property
                :tag label
                :type '(text :format " %v")
                :value-to-internal (lambda (_widget pvalue)
                                     (or (xmp-pvalue-as-text pvalue)
                                         ""))
                :value-to-external (lambda (_widget value)
                                     (if (and (stringp value)
                                              (not (string-empty-p value)))
                                         (xmp-pvalue-make-text value)
                                       nil))
                pvalue))
              ('LangAlt
               (prog1
                   (widget-create
                    'xmp-property
                    :tag label
                    :type 'xmp-lang-alt
                    :value-to-internal (lambda (_widget pvalue)
                                         (xmp-pvalue-as-lang-alt-alist pvalue))
                    :value-to-external (lambda (_widget value)
                                         (xmp-pvalue-from-lang-alt-alist value))
                    pvalue)
                 (insert "\n")))
              (_
               (widget-create
                'xmp-property
                :tag label
                :type '(sexp :format " %v")
                pvalue)))))))
    (put-text-property (widget-get widget :from)
                       (xmp-widget-property-tag-end widget)
                       'xmp-property prop-ename)
    widget))

;;;; Buffer

;;;###autoload
(defun xmp-editor-buffer-modified-p (&optional buffer)
  (when-let ((buffer (get-buffer (or buffer xmp-editor-default-buffer-name))))
    (buffer-modified-p buffer)))

;;;###autoload
(defun xmp-editor-buffer-modified-check (&optional buffer)
  (when (xmp-editor-buffer-modified-p buffer)
    (pop-to-buffer-same-window buffer)
    (error "There are unsaved modifications in the edit buffer.")))

(defun xmp-editor-modified-files-and-properties ()
  (cl-loop for file-info in xmp-editor-files
           for props = (xmp-editor-file-info-modified-properties file-info)
           when props
           collect (cons file-info props)))

(defun xmp-editor-save ()
  (interactive nil xmp-editor-mode)
  (let ((modified-files (xmp-editor-modified-files-and-properties)))
    (if (null modified-files)
        (message (xmp-msg "No modified properties"))
      (when (y-or-n-p (format
                       (xmp-msg "Save %d files with modified properties?")
                       (length modified-files)))
        (cl-loop for (file-info . props) in modified-files
                 do
                 (xmp-set-file-properties (xmp-editor-file-info-file file-info)
                                          props)
                 (xmp-editor-file-info-clear-modified-p file-info))
        (set-buffer-modified-p nil)))))

;;;; File Info

(defun xmp-editor-file-info (file property-widget-alist)
  (list file property-widget-alist))

(defun xmp-editor-file-info-file (file-info)
  (nth 0 file-info))

(defun xmp-editor-file-info-property-widget-alist (file-info)
  (nth 1 file-info))

(defun xmp-editor-file-info-modified-properties (file-info)
  (cl-loop for (prop-ename . widget)
           in (xmp-editor-file-info-property-widget-alist file-info)
           when (xmp-widget-property-modified-p widget)
           collect (cons prop-ename (widget-value widget))))

(defun xmp-editor-file-info-clear-modified-p (file-info)
  (cl-loop for (_prop-ename . widget)
           in (xmp-editor-file-info-property-widget-alist file-info)
           do (xmp-widget-property-clear-modified-p widget)))

;;;; Context

(defun xmp-editor-current-property-at ()
  (let ((pos (point))
        prop-ename)
    (while (and (null (setq prop-ename (get-text-property pos 'xmp-property)))
                (setq pos (previous-property-change pos))
                (null (get-text-property pos 'xmp-heading))))
    (cons prop-ename
          ;; beginning of property
          pos)))

(defun xmp-editor-current-property-begin ()
  (cdr (xmp-editor-current-property-at)))

(defun xmp-editor-current-file-at ()
  (let ((pos (point)))
    (while (and (not (get-text-property pos 'xmp-heading))
                (setq pos (previous-single-property-change pos 'xmp-heading))))
    (when pos
      (get-text-property pos 'xmp-heading-file))))

;;;; Navigation

(defun xmp-editor-previous-heading ()
  "Move to the previous heading line."
  (interactive nil xmp-editor-mode)
  (let ((pos (line-end-position 0)))
    (while (and (not (get-text-property pos 'xmp-heading))
                (setq pos (previous-single-property-change pos 'xmp-heading))))
    (when pos
      (goto-char pos)
      (forward-line 0))))

(defun xmp-editor-next-heading ()
  "Move to the next heading line."
  (interactive nil xmp-editor-mode)
  (let ((pos (line-beginning-position 2)))
    (while (and (not (get-text-property pos 'xmp-heading))
                (setq pos (next-single-property-change pos 'xmp-heading))))
    (when pos
      (goto-char pos))))

(defun xmp-editor-next-same-property ()
  "Move to the same property in the next file."
  (interactive nil xmp-editor-mode)
  (text-property-search-forward 'xmp-property
                                ;; prop-ename or nil
                                (car (xmp-editor-current-property-at)))
  (widget-forward 1))

(defun xmp-editor-previous-same-property ()
  "Move to the same property in the previous file."
  (interactive nil xmp-editor-mode)
  (let* ((current-property (xmp-editor-current-property-at))
         (current-property-name (car current-property))
         (current-property-begin (cdr current-property)))
    (when current-property-begin
      (goto-char current-property-begin))
    (text-property-search-backward 'xmp-property
                                   ;; prop-ename or nil
                                   current-property-name)
    (widget-forward 1)))

;;;;; Open File

(defcustom xmp-editor-open-target-file-function
  'find-file-other-window
  "A function used by the `xmp-editor-open-target-file-at' command."
  :group 'xmp
  :type '(choice
          (const find-file-other-window)
          (const find-file)
          (const org-open-file)
          (const browse-url)
          (function)))

(defun xmp-editor-open-target-file-at ()
  (interactive)
  (when-let ((file (xmp-editor-current-file-at)))
    (funcall xmp-editor-open-target-file-function file)))


;;;; End
(provide 'xmp-editor)
;;; xmp-editor.el ends here
