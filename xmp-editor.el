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

;;;; Basic Widgets
;;;;; XMP Property

;; Example:
;; (progn
;;   (pop-to-buffer (generate-new-buffer "*Widget Example*"))
;;   (setq test-wid
;;         (widget-create 'xmp-property :tag "Title"
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
;;   (widget-create 'xmp-lang-alt :tag "Title"
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
  (prog1
      (list (widget-create-child-and-convert
             widget 'text
             ;; Do not specify "%v " because :from marker type is t
             :format " %v "
             :size 20
             :value (xmp-widget-lang-alt-single-to-internal
                     (widget-get widget :value)))

            (widget-create-child-and-convert
             widget 'insert-button
             :action 'xmp-widget-lang-alt-single-insert)
            )
    (insert ?\n)))

(defun xmp-widget-lang-alt-single-to-internal (alist)
  (or (cdar alist) ""))

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


;;;;; Text List

;; Example:
;; (progn
;;   (pop-to-buffer (generate-new-buffer "*Widget Example*"))
;;   (setq test-wid
;;         (widget-create 'xmp-text-list :tag "Creators"
;;                        ;;'("Apple")
;;                        ;;'("Apple" "Orange")
;;                        ))
;;   (use-local-map widget-keymap)
;;   (widget-setup))

(define-widget 'xmp-text-list 'default
  "XMP Text List."
  :convert-widget 'widget-value-convert-widget
  :format "%v"
  :tag ""
  :value-create 'xmp-widget-text-list-value-create
  :value-get 'xmp-widget-text-list-value-get
  :validate 'widget-child-validate)

(defun xmp-widget-text-list-value-single-p (value)
  (null (cdr value)))

(defun xmp-widget-text-list-value-create (widget)
  (let* ((value (widget-get widget :value))
         (single-p (xmp-widget-text-list-value-single-p value)))
    (widget-put widget :xmp-single-p single-p)
    (widget-put widget :children
                (if single-p
                    (xmp-widget-text-list-single-value-create widget)
                  (xmp-widget-text-list-multiple-value-create widget)))))

(defun xmp-widget-text-list-value-get (widget)
  (if (widget-get widget :xmp-single-p)
      (xmp-widget-text-list-single-get-external-value widget)
    (xmp-widget-text-list-multiple-get-external-value widget)))

;; Single Item (x-default only)

(defun xmp-widget-text-list-single-value-create (widget)
  (prog1
      (list (widget-create-child-and-convert
             widget 'text
             ;; Do not specify "%v " because :from marker type is t
             :format " %v "
             :size 20
             :value (xmp-widget-text-list-single-to-internal
                     (widget-get widget :value)))

            (widget-create-child-and-convert
             widget 'insert-button
             :action 'xmp-widget-text-list-single-insert)
            )
    (insert ?\n)))

(defun xmp-widget-text-list-single-to-internal (list)
  (or (car list) ""))

(defun xmp-widget-text-list-single-get-external-value (widget)
  (let ((text (widget-value (car (widget-get widget :children)))))
    (if (string-empty-p text)
        nil
      (list text))))

(defun xmp-widget-text-list-single-insert (widget &rest _)
  (let* ((parent (widget-get widget :parent))
         (value (widget-value parent))
         (new-value
          (if (null value)
              (list "" "")
            (append value (list "")))))
    (widget-value-set parent new-value)
    (widget-setup)))

;; Multiple Items

(defun xmp-widget-text-list-multiple-value-create (widget)
  (list (widget-create-child-and-convert
         widget
         '(repeat
           :format "\n%v"
           ;;:type
           ;;(string :tag "" :format " %v")
           :notify xmp-widget-text-list-multiple-notify
           (string :tag "" :format " %v")
           )
         :value (xmp-widget-text-list-multiple-to-internal
                 (widget-get widget :value)))))

(defun xmp-widget-text-list-multiple-to-internal (list)
  list)

(defun xmp-widget-text-list-multiple-get-external-value (widget)
  (widget-value (car (widget-get widget :children))))

(defun xmp-widget-text-list-multiple-notify (widget _child &optional event)
  (let ((parent (widget-get widget :parent))
        (list (widget-value widget)))
    ;; Change to single type
    (when (= (length list) 1)
      (widget-value-set parent list)
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


;;;; XMP Property Widgets

(define-widget 'xmp-property-bag-text-csv 'xmp-property
  "XMP property of BagText type in CSV format."
  :type '(xmp-comma-separated-text :format " %v")
  :value-to-internal (lambda (_widget pvalue)
                       (xmp-pvalue-as-text-list pvalue))
  :value-to-external (lambda (_widget value)
                       (xmp-pvalue-make-bag-from-text-list
                        value)))

(define-widget 'xmp-property-text 'xmp-property
  "XMP property of Text type."
  :type '(text :format " %v")
  :value-to-internal (lambda (_widget pvalue)
                       (or (xmp-pvalue-as-text pvalue)
                           ""))
  :value-to-external (lambda (_widget value)
                       (if (and (stringp value)
                                (not (string-empty-p value)))
                           (xmp-pvalue-make-text value)
                         nil)))

(define-widget 'xmp-property-uri 'xmp-property
  "XMP property of URI type."
  :type '(text :format " %v")
  :value-to-internal (lambda (_widget pvalue)
                       (or (xmp-pvalue-as-uri pvalue)
                           ""))
  :value-to-external (lambda (_widget value)
                       (if (and (stringp value)
                                (not (string-empty-p value)))
                           (xmp-pvalue-make-uri value)
                         nil)))

(define-widget 'xmp-property-lang-alt 'xmp-property
  "XMP property of LangAlt type."
  :type 'xmp-lang-alt
  :value-to-internal (lambda (_widget pvalue)
                       (xmp-pvalue-as-lang-alt-alist pvalue))
  :value-to-external (lambda (_widget value)
                       (xmp-pvalue-from-lang-alt-alist value)))

(define-widget 'xmp-property-bag-text 'xmp-property
  "XMP property of BagText type."
  :type 'xmp-text-list
  :value-to-internal
  (lambda (_widget pvalue)
    (xmp-pvalue-as-text-list pvalue))
  :value-to-external
  (lambda (_widget value)
    (xmp-pvalue-make-bag-from-text-list value)))

(define-widget 'xmp-property-seq-text 'xmp-property
  "XMP property of SeqText type."
  :type 'xmp-text-list
  :value-to-internal
  (lambda (_widget pvalue)
    (xmp-pvalue-as-text-list pvalue))
  :value-to-external
  (lambda (_widget value)
    (xmp-pvalue-make-seq-from-text-list value)))

(define-widget 'xmp-property-sexp 'xmp-property
  "XMP property in SEXP format."
  :type '(sexp :format " %v"))

(defconst xmp-editor-property-name-widget-alist
  `((,xmp-dc:subject . xmp-property-bag-text-csv))
  "An alist of XMP property names and widget types.")

(defconst xmp-editor-property-type-widget-alist
  '(;; Text
    (Text . xmp-property-text)
    (URI . xmp-property-uri)
    (Real . xmp-property-text)
    (Integer . xmp-property-text)
    (AgentName . xmp-property-text)
    (GUID . xmp-property-text)
    (MIMEType . xmp-property-text)
    (Boolean . xmp-property-text)
    (Date . xmp-property-text)
    ;; LangAlt
    (LangAlt . xmp-property-lang-alt)
    ;; BagText
    (BagText . xmp-property-bag-text)
    (BagProperName . xmp-property-bag-text)
    (BagLocale . xmp-property-bag-text)
    (BagDate . xmp-property-bag-text)
    ;; SeqText
    (SeqText . xmp-property-seq-text)
    (SeqProperName . xmp-property-seq-text)
    (SeqLocale . xmp-property-seq-text)
    (SeqDate . xmp-property-seq-text))
  "An alist of XMP property types and widget types.")

(defun xmp-editor-property-widget-type (prop-ename type)
  "Return the widget type for editing the XMP property named PROP-ENAME."
  (or
   ;; From TYPE (widget type or property type)
   (and (symbolp type) (get type 'widget-type) type)
   (alist-get type xmp-editor-property-type-widget-alist)
   ;; From PROP-ENAME
   (xmp-xml-ename-alist-get prop-ename xmp-editor-property-name-widget-alist)
   (alist-get (xmp-defined-property-type prop-ename)
              xmp-editor-property-type-widget-alist)
   ;; Widget that can be used with any type
   'xmp-property-sexp))

;;;; Target Property Names

(defcustom xmp-editor-target-properties
  ;; ( <ename> <label> <type> )
  `((("http://purl.org/dc/elements/1.1/" . "title") nil nil)
    ;;(("http://purl.org/dc/elements/1.1/" . "creator") nil nil)
    (("http://purl.org/dc/elements/1.1/" . "subject") nil nil)
    (("http://purl.org/dc/elements/1.1/" . "description") nil nil)
    ;;(("http://purl.org/dc/elements/1.1/" . "date") nil nil)
    ;;(("http://ns.adobe.com/xap/1.0/" . "CreateDate") nil nil)
    (("http://ns.adobe.com/xap/1.0/" . "Label") nil nil)
    (("http://ns.adobe.com/xap/1.0/" . "Rating") nil nil)
    ;;(("https://ns.misohena.jp/xmp/" . "PlantName") "PlantName" xmp-property-bag-text-csv)
    ;;(("https://ns.misohena.jp/xmp/" . "Place") nil Text)
    ;;,(xmp-xml-ename (xmp-xml-ns-name "https://ns.misohena.jp/xmp/") "Phase")
    ;;all
    )
  "Properties to edit with the xmp-editor.
Used as the default value for `xmp-editor-open-files' etc.

Acceptable formats are:

  ( PROP-SPEC ... )
  `all'

If the symbol `all' is specified, all properties in the file will be
edited. However, this is not recommended because the presence of
unpredictable properties will disrupt the display.

When a list of PROP-SPEC is specified, the widgets that edit the
properties specified in PROP-SPEC will be displayed in the order they
appear in the list.

PROP-SPEC can be of one of the following formats:

  PROP-SPEC :
    ( ENAME LABEL TYPE )
    ENAME
    `all'

ENAME is the expanded name of a property. It is either a cons cell with
a namespace name string and a local name string, or an internal
representation of the expanded name created with the `xmp-xml-ename'
function.

  ENAME :
    ( NS-NAME-STRING . LOCAL-NAME-STRING )
    Result of `xmp-xml-ename'

LABEL is the tag string to be prepended to the edit widget. If nil, it
is generated from the ENAME.

  LABEL : STRING

TYPE is a symbol that specifies the type of widget. The symbol can be
either a widget type (xmp-property-*) or a property type (Text, LangAlt,
SeqBag, etc.). If this is nil, it will be determined from the
PROP-ENAME. (See: `xmp-editor-property-name-widget-alist' and
`xmp-editor-property-type-widget-alist')

  TYPE :
    WIDGET-TYPE-SYMBOL
    PROPERTY-TYPE-SYMBOL

If the symbol `all' is specified as PROP-SPEC, all properties present in
the file are specified, except those specified in other PROP-SPECs. This
is not recommended."
  :type '(repeat
          (choice
           ;; ( ENAME LABEL TYPE )
           (list
            (cons
             :tag "Property name"
             (string :tag "Namespace name (URI)") ;;TODO: Choice keyword?
             (string :tag "Property local name"))
            (choice :tag "Label"
                    (const :tag "Default" nil)
                    (string))
            (choice :tag "Type"
                    (const :tag "Default" nil)
                    (symbol)))
           ;; ENAME
           (cons
            :tag "Property name only"
            (string :tag "Namespace name (URI)") ;;TODO: Choice keyword?
            (string :tag "Property local name"))
           ;; `all'
           (const :tag "All loaded properties" all)))
  :group 'xmp)

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
                                    prop-spec-list
                                    buffer)
  "Display a UI for editing the XMP properties of the specified files.

FILES is the list of files to be edited. Sidecar files are automatically
excluded. If FILES is a string, it will be treated as a list with that
as its only element.

PROP-SPEC-LIST is a list that specifies information about the properties
to be edited. If it is nil or `default', the value of the variable
`xmp-editor-target-properties' is used. See the documentation of the
variable for details. In addition, specifying the symbol `default-all'
will specify `xmp-editor-target-properties' with `all' appended to the
end (if it does not already exist).

BUFFER is the buffer name or buffer object to use to display the
editor. If it is nil, `xmp-editor-default-buffer-name' is used."
  (when (stringp files)
    (setq files (list files)))
  ;; TODO: Warn if there is an existing editor
  (xmp-editor-create-files-buffer (or buffer xmp-editor-default-buffer-name)
                                  (seq-remove #'xmp-sidecar-file-p files)
                                  prop-spec-list))

(defun xmp-editor-create-files-buffer (buffer files prop-spec-list)
  (let ((dir default-directory))
    (pop-to-buffer-same-window buffer)
    (xmp-editor-initialize-files-buffer files prop-spec-list dir)))

(defun xmp-editor-initialize-files-buffer (files prop-spec-list &optional dir)
  ;; Activating major mode does following:
  ;; - Call (kill-all-local-variables)
  ;; - Call (use-local-map xmp-editor-mode-map)
  (xmp-editor-mode)
  (erase-buffer)
  (remove-overlays)

  (when dir
    (setq default-directory dir))

  (xmp-editor-insert-files-header)
  (xmp-editor-insert-files-body files prop-spec-list)
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

(defun xmp-editor-insert-files-body (files prop-spec-list)
  ;; Expand special keyword
  (cond
   ;; Default properties
   ((or (eq prop-spec-list nil)
        (eq prop-spec-list 'default))
    (setq prop-spec-list xmp-editor-target-properties))
   ;; Default properties and all properties in file
   ((eq prop-spec-list 'default-all)
    (setq prop-spec-list
          (if (and (listp xmp-editor-target-properties)
                   (not (memq 'all xmp-editor-target-properties)))
              (append xmp-editor-target-properties
                      (list 'all))
            ;; `all' is already included
            xmp-editor-target-properties))))

  (let ((xmp-editor-image-file-regexp-cache
         (xmp-editor-create-image-file-regexp))
        (prop-ename-list
         (xmp-editor-make-prop-ename-list-to-read prop-spec-list))
        (prop-spec-list
         (xmp-editor-align-labels
          (xmp-editor-complete-prop-spec-list
           prop-spec-list
           xmp-default-ns-name-prefix-alist))))

    (setq-local xmp-editor-files
                (cl-loop for file in files
                         ;;unless (xmp-sidecar-file-p file)
                         collect
                         (prog1 (xmp-editor-insert-file file
                                                        prop-spec-list
                                                        prop-ename-list)
                           (insert "\n"))))))

(defun xmp-editor-insert-file (file prop-spec-list prop-ename-list)
  (setq file (expand-file-name file))
  (xmp-editor-insert-file-heading file)
  (when (xmp-editor-image-file-p file)
    (xmp-editor-insert-file-thumbnail file))
  (xmp-editor-file-info
   file
   (xmp-editor-insert-file-properties file prop-spec-list prop-ename-list)))

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

(defun xmp-editor-insert-file-properties (file prop-spec-list prop-ename-list)
  (let* ((ns-name-prefix-alist (xmp-xml-standard-ns-name-prefix-alist))
         (props (xmp-enumerate-file-properties file prop-ename-list
                                               ns-name-prefix-alist)))

    ;; Expand properties not specified in PROP-SPEC-LIST.
    (unless prop-ename-list
      (setq prop-spec-list
            (xmp-editor-align-labels
             (xmp-editor-expand-all-in-prop-spec-list prop-spec-list
                                                      props
                                                      ns-name-prefix-alist))))

    ;; Return property widget alist
    (xmp-editor-insert-properties props prop-spec-list)))

(defun xmp-editor-insert-properties (props prop-spec-list)
  (cl-loop for prop-spec in prop-spec-list
           for prop-ename = (xmp-editor-prop-spec-ename prop-spec)
           for label = (xmp-editor-prop-spec-label prop-spec)
           for widget-type = (xmp-editor-prop-spec-type prop-spec)
           collect (cons
                    prop-ename
                    (xmp-editor-insert-property prop-ename
                                                props
                                                label
                                                widget-type))))

(defun xmp-editor-insert-property (prop-ename props label widget-type)
  (let* ((pvalue (xmp-xml-ename-alist-get prop-ename props))
         (widget (widget-create widget-type :tag label pvalue)))
    ;; Mark label
    (put-text-property (widget-get widget :from)
                       (xmp-widget-property-tag-end widget)
                       'xmp-property prop-ename)
    widget))

(defun xmp-editor-property-label (prop-ename ns-name-prefix-alist)
  ;; TODO: Generate more user-friendly text. Add xmp-editor-property-label-alist?
  ;; TODO: Move to xmp-commands.el or xmp.el?
  (let ((prefix
         (or (xmp-default-namespace-prefix (xmp-xml-ename-ns prop-ename))
             (xmp-xml-ns-name-to-prefix (xmp-xml-ename-ns prop-ename)
                                        ns-name-prefix-alist
                                        t)
             ;; (xmp-xml-ns-name-string (xmp-xml-ename-ns prop-ename)) ;;Too long?
             )))
    (if prefix
        (concat prefix ":" (xmp-xml-ename-local prop-ename))
      (xmp-xml-ename-local prop-ename))))

;;;;; prop-spec-list

;; See: the document of the `xmp-editor-target-properties' variable.

;; <prop-spec-list> : all | ( <prop-spec>... )
;; <prop-spec> : all | <ename> | ( <ename> <label> <widget-type> )
;; <ename> : ( <ns-name-str> . <local-name-str> ) | <Result of `xmp-xml-ename'>

(defun xmp-editor-prop-spec-ename (prop-spec) (nth 0 prop-spec))
(defun xmp-editor-prop-spec-label (prop-spec) (nth 1 prop-spec))
(defun xmp-editor-prop-spec-type (prop-spec) (nth 2 prop-spec))

(defun xmp-editor-make-prop-ename-list-to-read (prop-spec-list)
  "Create a prop-ename-list to pass to `xmp-enumerate-file-properties'.
Return nil if all properties should be read."
  (when (listp prop-spec-list)
    (cl-loop for prop-spec in prop-spec-list
             unless (listp prop-spec) return nil ;; Return nil if contains 'all
             collect (if (or (xmp-xml-ename-p prop-spec)
                             (and (consp prop-spec)
                                  (stringp (car prop-spec))
                                  (stringp (cdr prop-spec))))
                         ;; ENAME
                         (xmp-xml-ename-ensure prop-spec)
                       ;; ( ENAME LABEL TYPE )
                       (xmp-xml-ename-ensure
                        (xmp-editor-prop-spec-ename prop-spec))))))

(defun xmp-editor-complete-prop-spec-list (prop-spec-list ns-name-prefix-alist)
  "Fill in the missing information in PROP-SPEC-LIST.

NS-NAME-PREFIX-ALIST is an alist used to determine namespace prefixes
when generating labels."
  (cond
   ((listp prop-spec-list)
    (cl-loop for prop-spec in prop-spec-list
             collect (xmp-editor-complete-prop-spec prop-spec
                                                    ns-name-prefix-alist)))
   ((eq prop-spec-list 'all)
    (list 'all))))

(defun xmp-editor-complete-prop-spec (prop-spec ns-name-prefix-alist)
  "Fill in the missing information in PROP-SPEC.

NS-NAME-PREFIX-ALIST is an alist used to determine namespace prefixes
when generating labels."
  (cond
   ;; nil : ignore
   ((null prop-spec))
   ;; ENAME
   ((or (xmp-xml-ename-p prop-spec)
        (and (consp prop-spec)
             (stringp (car prop-spec))
             (stringp (cdr prop-spec))))
    (let* ((prop-ename (xmp-xml-ename-ensure prop-spec))
           (label (xmp-editor-property-label prop-ename ns-name-prefix-alist))
           (widget-type (xmp-editor-property-widget-type prop-ename nil)))
      (list prop-ename label widget-type)))
   ;; ( ENAME LABEL TYPE )
   ((listp prop-spec)
    (let* ((prop-ename
            (xmp-xml-ename-ensure (xmp-editor-prop-spec-ename prop-spec)))
           (label
            (or (xmp-editor-prop-spec-label prop-spec)
                (xmp-editor-property-label prop-ename ns-name-prefix-alist)))
           (widget-type
            (xmp-editor-property-widget-type
             prop-ename
             (xmp-editor-prop-spec-type prop-spec))))
      (list prop-ename label widget-type)))
   (t
    ;; 'all
    prop-spec)))

(defun xmp-editor-expand-all-in-prop-spec-list (prop-spec-list
                                                loaded-props-alist
                                                ns-name-prefix-alist)
  "Replace the `all' keyword in PROP-SPEC-LIST with all properties actually
read.

LOADED-PROPS-ALIST is an alist of the properties that were actually read.

NS-NAME-PREFIX-ALIST is an alist used to determine namespace prefixes
when generating labels."
  (let ((specified-prop-ename-list
         (cl-loop for prop-spec in prop-spec-list
                  when (listp prop-spec)
                  collect (xmp-editor-prop-spec-ename prop-spec)))
        (result))
    (dolist (prop-spec prop-spec-list)
      (cond
       ((listp prop-spec)
        (push prop-spec result))
       ((eq prop-spec 'all)
        (dolist (prop loaded-props-alist)
          (let ((prop-ename (car prop)))
            (unless (xmp-xml-ename-member prop-ename specified-prop-ename-list)
              (push (xmp-editor-complete-prop-spec
                     (list
                      prop-ename
                      nil
                      (xmp-editor-infer-property-type-from-pvalue (cdr prop)))
                     ns-name-prefix-alist)
                    result)))))
       (t
        ;; Ignore?
        )))
    (nreverse result)))

(defun xmp-editor-infer-property-type-from-pvalue (pvalue)
  (cond
   ((xmp-pvalue-text-p pvalue)
    (if (xmp-pvalue-pure-p pvalue) 'Text nil))
   ((xmp-pvalue-uri-p pvalue)
    (if (xmp-pvalue-pure-p pvalue) 'URI nil))
   ((xmp-pvalue-array-p pvalue)
    (if (and (xmp-pvalue-pure-p pvalue)
             (seq-every-p (lambda (item) (and (xmp-pvalue-text-p item)
                                              (xmp-pvalue-pure-p item)))
                          (xmp-pvalue-as-list pvalue)))
        (let ((arr-type (xmp-pvalue-array-type pvalue)))
          (cond
           ((xmp-xml-ename-equal arr-type xmp-rdf:Seq) 'SeqText)
           ((xmp-xml-ename-equal arr-type xmp-rdf:Bag) 'BagText)
           (t nil)))
      nil))
   ;; Unknown
   (t nil)))

(defun xmp-editor-align-labels (prop-spec-list)
  "Align all label strings in PROP-SPEC-LIST to the same width."
  (let ((max-width (cl-loop for prop-spec in prop-spec-list
                            unless (listp prop-spec)
                            return nil
                            for label = (xmp-editor-prop-spec-label prop-spec)
                            maximize (string-width label))))
    (if max-width
        (cl-loop for prop-spec in prop-spec-list
                 for label = (xmp-editor-prop-spec-label prop-spec)
                 for aligned-label = (concat
                                      (make-string
                                       (max 0
                                            (- max-width (string-width label)))
                                       ? )
                                      label)
                 collect (list (xmp-editor-prop-spec-ename prop-spec)
                               aligned-label
                               (xmp-editor-prop-spec-type prop-spec)))
      prop-spec-list)))


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

;;;;; Open Target File

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
