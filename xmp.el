;;; xmp.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 AKIYAMA Kouhei

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

;; Get properties from file:
;;
;;  (xmp-file-get-properties "test/xmp-test-value-types.xmp" 'all nil t)
;;
;;  (xmp-file-get-properties "test/xmp-test-uzumaki.jpg" 'all)
;;
;;  (xmp-file-get-properties "XMPSpecificationPart1.pdf" 'all)
;;
;;  (xmp-file-get-properties "test/xmp-test-uzumaki.jpg"
;;    (list (xmp-xml-ename xmp-xmp: "Rating")
;;          (xmp-xml-ename xmp-dc: "title")))
;;
;;  (xmp-pvalue-as-text
;;    (xmp-file-get-property "test/xmp-test-uzumaki.jpg"
;;      (xmp-xml-ename xmp-xmp: "Rating")))

;; Set properties to file:
;;
;;  (xmp-file-set-properties "tmp-example.xmp"
;;    (list
;;      (cons xmp-xmp:Rating "5")
;;      (cons xmp-dc:title
;;            (xmp-pvalue-make-alt
;;              (list
;;                (xmp-pvalue-make-text
;;                  "Test Title"
;;                  (list (xmp-pvalue-make-named xmp-xml:lang 'text "x-default")))
;;                (xmp-pvalue-make-text
;;                  "Test Title"
;;                  (list (xmp-pvalue-make-named xmp-xml:lang 'text "en")))
;;                (xmp-pvalue-make-text
;;                  "テストタイトル"
;;                  (list (xmp-pvalue-make-named xmp-xml:lang 'text "ja"))))))))
;;
;;  (xmp-file-set-property "tmp-example.xmp" xmp-xmp:Rating "3")
;;

;;;; References:

;; XMP1 : Extensible Metadata Platform (XMP) Specification Part1 [April, 2012]
;;        https://developer.adobe.com/xmp/docs/XMPSpecifications/
;; XMP2 : Extensible Metadata Platform (XMP) Specification Part2 [Feb, 2022]
;;        https://developer.adobe.com/xmp/docs/XMPSpecifications/
;; XMP3 : Extensible Metadata Platform (XMP) Specification Part3 [Jan, 2020]
;;        https://developer.adobe.com/xmp/docs/XMPSpecifications/


;;; Code:

(require 'cl-lib)
(require 'xmp-xml)
(require 'xmp-file-reader)

;;;; Message Text Translation

(defun xmp-msg (str)
  "Process message text STR for users.

The current implementation returns STR as is. If you want to translate
the message text into other languages, please rewrite this function."
  str)

(defun xmp-msg-n (str)
  "Return STR as is.

This function is intended to be used as a marker for detecting message
text from source code."
  str)

;;;; Customize

(defgroup xmp nil
  "Extensible Metadata Platform."
  :tag "XMP"
  :prefix "xmp-"
  :group 'files)

;;;; Namespace Information
;;;;; Predefined Namespaces
(eval-and-compile
  (defconst xmp-predefined-namespaces
    ;; (<namespace name string> . (<prefix> ...more info))
    '(("elxmp://el-xmp/xmlns/"
       "elxmp")
      ("adobe:ns:meta/"
       "x") ;;[XMP1 7.3.3]
      ("http://www.w3.org/1999/02/22-rdf-syntax-ns#"
       "rdf") ;;[XMP1 6.2]
      ("http://purl.org/dc/elements/1.1/"
       "dc") ;;[XMP1 8.3]
      ("http://ns.adobe.com/xap/1.0/"
       "xmp") ;;[XMP1 8.4] [XMP2 2.1]
      ("http://ns.adobe.com/xap/1.0/rights/"
       "xmpRights") ;;[XMP1 8.5]
      ("http://ns.adobe.com/xap/1.0/mm/"
       "xmpMM") ;;[XMP1 8.6] [XMP2 2.2]
      ("http://ns.adobe.com/xmp/Identifier/qual/1.0/"
       "xmpidq") ;;[XMP1 8.7]
      ("http://ns.adobe.com/xap/1.0/bj/"
       "xmpBJ") ;;[XMP2 2.3]
      ("http://ns.adobe.com/xap/1.0/t/pg/"
       "xmpTPg") ;;[XMP2 2.4]
      ("http://ns.adobe.com/xmp/1.0/DynamicMedia/"
       "xmpDM") ;;[XMP2 1.2.6] [XMP2 2.5]
      ("http://ns.adobe.com/pdf/1.3/"
       "pdf") ;;[XMP2 3.1]
      ("http://ns.adobe.com/photoshop/1.0/"
       "photoshop") ;;[XMP2 3.2]
      ("http://ns.adobe.com/camera-raw-settings/1.0/"
       "crs") ;;[XMP2 3.3]
      ("http://ns.adobe.com/exif/1.0/"
       "exif") ;;[XMP2 3.4]

      ("http://ns.adobe.com/xap/1.0/g/"
       "xmpG") ;;[XMP2 1.2.2.1]
      ("http://ns.adobe.com/xap/1.0/sType/Dimensions#"
       "stDim") ;;[XMP2 1.2.2.2]
      ("http:ns.adobe.com/xap/1.0/sType/Font#"
       "stFnt") ;;[XMP2 1.2.2.3]
      ("http://ns.adobe.com/xap/1.0/g/img/"
       "xmpGImg") ;;[XMP2 1.2.2.4]
      ("http://ns.adobe.com/xap/1.0/sType/ResourceEvent#"
       "stEvt") ;;[XMP2 1.2.4]
      ("http://ns.adobe.com/xap/1.0/sType/ResourceRef#"
       "stRef") ;;[XMP1 8.2.2.9] [XMP2 1.2.4.1]
      ("http://ns.adobe.com/xap/1.0/sType/Version#"
       "stVer") ;;[XMP2 1.2.4.2]
      ("http://ns.adobe.com/xap/1.0/sType/Job#"
       "stJob") ;;[XMP2 1.2.5.1]

      ("http://cipa.jp/exif/1.0/"
       "exifEX") ;;[CIPA DC-010-2024 5.1]
      ("http://ns.adobe.com/tiff/1.0/"
       "tiff") ;;https://developer.adobe.com/xmp/docs/XMPNamespaces/tiff/
      ("http://iptc.org/std/Iptc4xmpCore/1.0/xmlns/"
       "Iptc4xmpCore") ;; https://developer.adobe.com/xmp/docs/XMPNamespaces/Iptc4xmpCore/
      ))

  ;; Register ns-name-prefix-alist to xmp-xml.el
  (xmp-xml-register-ns-name-prefix-group
   'xmp-predefined
   (mapcar (lambda (ns-info)
             (cons (xmp-xml-ns-name (nth 0 ns-info)) (nth 1 ns-info)))
           xmp-predefined-namespaces)
   0)
  ;; (xmp-xml-default-ns-prefix :http://ns.adobe.com/xap/1.0/) => "xmp"
  ;; (xmp-xml-default-ns-prefix "http://ns.adobe.com/xap/1.0/") => "xmp"
  )

;; Define namespace name variables
;;     (defconst xmp-<ns-prefix>: <ns-name>)
;;   (defconst xmp-x: ...)
;;   (defconst xmp-xmp: ...)
;;   (defconst xmp-rdf: ...)
;;   etc.
(defmacro xmp-define-ns-const ()
  `(progn
     ,@(cl-loop for (ns-name . ns-info) in xmp-predefined-namespaces
                for ns-prefix = (car ns-info)
                collect
                `(defconst ,(intern (format "xmp-%s:" ns-prefix))
                   (xmp-xml-ns-name ,ns-name)
                   ;; TODO: Generate docstring
                   ))))
(xmp-define-ns-const)

;; Define predefined namespace prefix alist variable
;;     (defconst xmp-predefined-ns-name-prefix-alist
;;       '((:adobe:ns:meta/ . "x")
;;         (:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "rdf") ...))
(defmacro xmp-define-ns-namespaces-const ()
  `(defconst xmp-predefined-ns-name-prefix-alist
     (list
      ,@(cl-loop for (ns-name . ns-info) in xmp-predefined-namespaces
                 for ns-prefix = (car ns-info)
                 collect `(cons (xmp-xml-ns-name ,ns-name) ,ns-prefix)))))
(xmp-define-ns-namespaces-const)

;;;;; User Defined Namespaces

(defvar xmp-user-defined-namespaces)

(defun xmp-user-defined-namespaces-update ()
  ;; Register ns-name-prefix-alist to xmp-xml.el
  (xmp-xml-register-ns-name-prefix-group
   'xmp-user
   (mapcar (lambda (ns-info)
             (cons (xmp-xml-ns-name (nth 0 ns-info)) (nth 1 ns-info)))
           xmp-user-defined-namespaces)
   -1))

(defcustom xmp-user-defined-namespaces nil
  "A list of namespaces added by user.

Specify namespaces that are not included in `xmp-predefined-namespaces'."
  :group 'xmp
  :type '(repeat (list (string :tag "Namespace Name (URI)")
                       (string :tag "Default Prefix")))
  :set (lambda (var val)
         (set var val)
         ;; Update prefix alist
         (xmp-user-defined-namespaces-update)))


;;;; Predefined Element and Attribute Names

(eval-and-compile
  (defconst xmp-predefined-names
    ;; ( (<namespace name string> . ( (<local-name> ...more info)... ) )... )
    '(("x"
       ("xmpmeta") ;; Element
       ("xmptk")) ;; Attribute
      ("rdf"
       ("RDF")
       ("Description")
       ("Bag")
       ("Seq")
       ("Alt")
       ("li")
       ("value") ;; Element or Attribute
       ("type") ;; Element or Attribute
       ("about") ;; Attribute
       ("resource")
       ("parseType")
       ("datatype")
       ("ID")
       ("nodeID")))))

;; Define expanded name variables for elements/attributes
;;   (defconst xmp-x:metadata ...)
;;   (defconst xmp-rdf:RDF ...)
;;   etc.
(defmacro xmp-define-predefined-names ()
  `(xmp-define-predefined-names-1 ,xmp-predefined-names))

(defmacro xmp-define-predefined-names-1 (predefined-names)
  `(progn
     ,@(cl-loop for (ns-prefix . name-info-list) in predefined-names
                nconc
                (cl-loop for name-info in name-info-list
                         for name = (car name-info)
                         for var = (intern (format "xmp-%s:%s" ns-prefix name))
                         for ns-var = (intern (format "xmp-%s:" ns-prefix))
                         collect
                         `(defconst ,var (xmp-xml-ename ,ns-var ,name))))))

(xmp-define-predefined-names)

(defconst xmp-names:lang-ID-nodeID (list xmp-xml:lang
                                         xmp-rdf:ID
                                         xmp-rdf:nodeID))

;;;; Property Type Information

(defconst xmp-predefined-property-types
  '(Text URI Boolean Real Integer GUID Date AgentName RenditionClass
         ResourceRef MIMEType LangAlt
         BagText BagProperName BagLocale BagDate
         SeqText SeqProperName SeqLocale SeqDate))

;;;; Property Information
;;;;; Predefined Properties

(eval-and-compile
  (defconst xmp-predefined-properties
    ;; ( (<namespace prefix string> . ( (<local-name> ...more info)... ) )... )
    '(;; [XMP1 8.2.2.9 ResourceRef]
      ("stRef"
       ("documentID" GUID)
       ("filePath" URI)
       ("instanceID" GUID)
       ("renditionClass" RenditionClass)
       ("renditionParams" Text))
      ;; [XMP1 8.3 Dublin Core namespace]
      ("dc"
       ("contributor" BagProperName)
       ("coverage" Text)
       ("creator" SeqProperName)
       ("date" SeqDate)
       ("description" LangAlt)
       ("format" MIMEType)
       ("identifier" Text)
       ("language" BagLocale)
       ("publisher" BagProperName)
       ("relation" BagText)
       ("rights" LangAlt)
       ("source" Text)
       ("subject" BagText)
       ("title" LangAlt)
       ("type" BagText))
      ;; [XMP1 8.4 XMP namespace]
      ("xmp"
       ("CreateDate" Date)
       ("CreatorTool" AgentName)
       ("Identifier" BagText)
       ("Label" Text)
       ("MetadataDate" Date)
       ("ModifyDate" Date)
       ("Rating" Real))
      ;; [XMP1 8.5 XMP Rights Management namespace]
      ("xmpRights"
       ("Certificate" Text)
       ("Marked" Boolean)
       ("Owner" BagProperName)
       ("UsageTerms" LangAlt)
       ("WebStatement" Text))
      ;; [XMP1 8.6 XMP Media Management namespace]
      ("xmpMM"
       ("DerivedFrom" ResourceRef)
       ("DocumentID" GUID)
       ("InstanceID" GUID)
       ("OriginalDocumentID" GUID)
       ("RenditionClass" RenditionClass)
       ("RenditionParams" Text))
      ;; [XMP1 8.7 xmpidq namespace]
      ("xmpidq"
       ("Scheme" Text))
      )))

;; Define expanded name variables for properties
(defmacro xmp-define-predefined-properties ()
  `(xmp-define-predefined-names-1 ,xmp-predefined-properties))
(xmp-define-predefined-properties)

;;;;; User Defined Properties

(defcustom xmp-user-defined-properties nil
  "A list of XMP properties added by user.

Specify properties that are not included in `xmp-predefined-properties'.

When adding a namespace to this variable, register the namespace
information in `xmp-user-defined-namespaces'."
  :group 'xmp
  :type `(alist
          :tag "Namespaces and Properties Alist"
          :key-type (string :tag "Namespace Prefix")
          :value-type (repeat
                       :tag "Properties"
                       (list
                        (string :tag "Name")
                        (choice :tag "Type"
                                symbol
                                ,@(mapcar
                                   (lambda (s) `(const ,s))
                                   xmp-predefined-property-types))))))

;;;;; Defined Property Information

(defun xmp-defined-property-type--get (ename prop-info-alist)
  (when-let* ((ns-prefix-str (xmp-xml-default-ns-prefix
                              (xmp-xml-ename-ns ename)))
              (ns-info (assoc ns-prefix-str
                              prop-info-alist
                              #'string=))
              (prop-info (assoc (xmp-xml-ename-local ename)
                                (cdr ns-info)
                                #'string=)))
    (nth 1 prop-info)))

(defun xmp-defined-property-type (ename)
  (or (xmp-defined-property-type--get ename xmp-user-defined-properties)
      (xmp-defined-property-type--get ename xmp-predefined-properties)))
;; TEST: (xmp-defined-property-type xmp-xmp:Rating) => Real


;;;; DOM Tree Creation

(defun xmp-empty-dom ()
  "Create an empty XMP XML DOM with no properties.

The root node is an x:xmpmeta element that contains one child rdf:RDF
element that contains an empty rdf:Description element."
  ;; <x:xmpmeta>
  (xmp-xml-element
   xmp-x:xmpmeta
   nil
   ;; <rdf:RDF>
   (list
    (xmp-xml-element
     xmp-rdf:RDF
     nil
     ;; <rdf:Description>
     (list
      (xmp-empty-top-description))))))
;; EXAMPLE: (xmp-xml-print nil (xmp-empty-dom) xmp-default-ns-name-prefix-alist)
;; EXAMPLE: (xmp-xml-collect-used-ns (xmp-empty-dom))

(defun xmp-empty-top-description (&optional about)
  "Create an empty top-level rdf:Description element.

The element is intended to be added to an rdf:RDF element.

The element has an about attribute whose value is ABOUT. If omitted, it
defaults to the empty string\"\"."
  (xmp-xml-element xmp-rdf:Description
                   (list
                    (xmp-xml-attr xmp-rdf:about (or about "")))))

;;;; RDF Element (XMP DOM)

(defun xmp-find-rdf (dom)
  "Return rdf:RDF element in DOM.

If DOM is an x:xmpmeta element, return the first rdf:RDF element among
its children. If DOM is itself an rdf:RDF element, return it as is."
  (cond
   ;; Unwrap <x:xmpmeta> ... <rdf:RDF>...</rdf:RDF> ... </x:xmpmeta>
   ((xmp-xml-element-enamed-p dom xmp-x:xmpmeta)
    (xmp-xml-element-child-find-by-ename dom xmp-rdf:RDF))
   ;; <rdf:RDF> ... </rdf:RDF>
   ((xmp-xml-element-enamed-p dom xmp-rdf:RDF)
    dom)))

(defun xmp-find-description (dom &optional prop-ename about)
  "Return an top-level rdf:Description element from DOM that has a
property named PROP-ENAME.

The property specified by PROP-ENAME is searched from both attributes
and child elements. If PROP-ENAME is nil, return the first top-level
rdf:Description element.

ABOUT is a string that matches the about attribute of rdf:Description
elements. If omitted, it is the empty string."
  (when-let ((rdf (xmp-find-rdf dom)))
    (seq-find (lambda (node)
                (and
                 (xmp-target-description-p node about)
                 (or
                  (null prop-ename)
                  ;; From the DESC's attributes
                  ;; (simple, non-URI, unqualified values)
                  ;; [XMP1 7.9.2.2]
                  (xmp-xml-element-attr-find-by-ename node prop-ename)
                  ;; From the DESC's children
                  ;; [XMP1 C.2.5]
                  (xmp-xml-element-child-find-by-ename node prop-ename))))
              (xmp-xml-element-children rdf))))

(defun xmp-remove-all-descriptions (dom &optional about)
  (when-let ((rdf (xmp-find-rdf dom)))
    (xmp-xml-element-child-remove-if
     rdf
     (lambda (child-node)
       (xmp-target-description-p child-node about)))
    (null (xmp-xml-element-children rdf))))

(defun xmp-get-properties (dom prop-ename-list &optional about noerror)
  "Return a list of XMP properties contained in DOM.

Each element in the returned list is a cons cells whose car is the
expanded name of the property and whose cdr is the pvalue (the parsed
value: as returned by `xmp-parse-property-element').

PROP-ENAME-LIST is a list of the expanded names of the properties to
retrieve, or the symbol `all'. nil means to retrieve none. The symbol
`all' means to enumerate all properties. The order or duplication of
properties in the list does not affect the value returned.

ABOUT specifies a string that matches the about attribute of Description
elements. Get properties only from matching Description
elements. Specifying nil is the same as specifying an empty string.

If NOERROR is non-nil, then nil is returned if an error occurs, whenever
possible."
  (cl-loop for prop-elt in (xmp-get-property-elements dom prop-ename-list
                                                      about)
           for pvalue = (xmp-parse-property-element prop-elt noerror)
           when pvalue ;; <ns:Prop /> => nil or parse error (noerror is non-nil)
           collect pvalue))

(defun xmp-get-property-elements (dom prop-ename-list &optional about)
  "Return a list of elements that represent the XMP properties contained in
DOM.

The elements of the list are child elements of the top-level
Descriptions (directly under the RDF element), except that if properties
are expressed in the form of attribute values of top-level Description
elements (property attributes), a temporary property element is created
and returned.

PROP-ENAME-LIST is a list of the expanded names of the properties to
retrieve or the symbol `all'. nil means to retrieve none. The symbol
`all' means to enumerate all properties. The order or duplication of
properties in the list does not affect the value returned.

ABOUT specifies a string that matches the about attribute of Description
elements. Get properties only from matching Description
elements. Specifying nil is the same as specifying an empty string."
  (when prop-ename-list
    (when-let ((rdf (xmp-find-rdf dom)))
      ;; At the top level there are only Description elements.
      ;; [XMP1 C.2.3]
      (cl-loop for desc in (xmp-xml-element-children rdf)
               when (xmp-target-description-p desc about)
               nconc (xmp-desc-get-property-elements desc prop-ename-list)))))

(defun xmp-get-property-element (dom prop-ename &optional about)
  "Return the property element with the name PROP-ENAME in DOM, or nil
if none.

ABOUT specifies a string that matches the about attribute of Description
elements. Get property only from matching Description
elements. Specifying nil is the same as specifying an empty string."
  (when-let ((rdf (xmp-find-rdf dom)))
    (seq-some (lambda (desc)
                (when (xmp-target-description-p desc about)
                  (xmp-desc-get-property-element desc prop-ename)))
              (xmp-xml-element-children rdf))))

(defun xmp-set-property-elements-if-not-exists (dom prop-elem-list
                                                    &optional about)
  "Insert the property elements specified by PROP-ELEM-LIST into the
DOM. However, if a property with the same name already exists in the
DOM, do not insert it and discard it."
  (when prop-elem-list
    (when-let ((rdf (xmp-find-rdf dom)))
      ;; Prepare for `cl-delete-if' and `xmp-xml-element-nconc-last'
      (setq prop-elem-list (copy-sequence prop-elem-list))

      ;; Remove property elements from PROP-ELEM-LIST that are already in DOM
      (let ((children (xmp-xml-element-children rdf)))
        ;; For each rdf:Description
        (while (and children prop-elem-list)
          (let ((desc (pop children)))
            (when (xmp-target-description-p desc about)
              ;; For each properties
              (xmp-desc-some-property-elements
               desc
               (lambda (prop-ename _place _elem _attr)
                 ;; Remove elements with same property name from PROP-ELEM-LIST
                 (setq prop-elem-list
                       (cl-delete-if
                        (lambda (elem2)
                          (xmp-xml-ename-equal (xmp-xml-element-ename elem2)
                                               prop-ename))
                        prop-elem-list))
                 ;; Stop if PROP-ELEM-LIST is empty
                 (null prop-elem-list))
               'all)))))

      ;; Insert property elements
      (let ((desc (or (xmp-find-description dom nil about)
                      (xmp-xml-element-insert-last
                       dom
                       (xmp-empty-top-description about)))))
        (xmp-xml-element-nconc-last desc prop-elem-list)

        ;; Insert namespace declarations that are used and do not exist
        (let (used-ns-list nsdecls)
          ;; Collect used namespace names from PROP-ELEM-LIST
          (dolist (prop-elem prop-elem-list)
            (setq used-ns-list
                  (xmp-xml-collect-used-ns prop-elem used-ns-list)))

          ;; Collect namespace declarations from the root to the desc element
          (setq nsdecls
                (nconc
                 (xmp-xml-collect-nsdecls-on-node desc)
                 (xmp-xml-collect-nsdecls-on-node rdf)
                 (unless (eq dom rdf)
                   (xmp-xml-collect-nsdecls-on-node dom))))

          ;; Insert missing namespace declarations
          (dolist (ns-name used-ns-list)
            (unless (seq-some (lambda (nsdecl) (equal (car nsdecl) ns-name))
                              nsdecls)
              (when-let ((prefix (xmp-xml-default-ns-prefix ns-name)))
                (xmp-xml-insert-nsdecl dom ns-name prefix)))))))))

(defun xmp-dump-properties (stream dom &optional about sort)
  "Output XMP properties contained in DOM to STREAM in a
user-friendly format.

ABOUT specifies a string that matches the about attribute of Description
elements. Get properties only from matching Description
elements. Specifying nil is the same as specifying an empty string."
  (let ((props (xmp-get-properties dom 'all about t)))
    (when sort
      (setq props (xmp-xml-ename-alist-sort props)))

    (xmp-dump-named-pvalue-list stream props (xmp-xml-collect-nsdecls dom) 0)))
;; EXAMPLE: (xmp-dump-properties nil (xmp-file-read-rdf "test/xmp-test-uzumaki.jpg") nil t)

;;;; Description Element

(defun xmp-target-description-p (node about)
  "Return non-nil if NODE is a rdf:Description element to be processed.

ABOUT specifies a string that matches the about attribute."
  (and (xmp-xml-element-enamed-p node xmp-rdf:Description)
       (equal
        ;; If there is no "about" attribute, treat it as an
        ;; empty string.  [XMP1 7.4][XMP1 C.2.4]
        (or (xmp-xml-element-attr-value node xmp-rdf:about) "")
        (or about ""))))

(defun xmp-desc-get-property-elements (desc prop-ename-list)
  "Return the list of XMP property elements present within the
rdf:Description element DESC.

PROP-ENAME-LIST is a list of the expanded names of the properties to
retrieve or the symbol `all'. nil means to retrieve none. The symbol
`all' means to enumerate all properties. The order or duplication of
properties in the list does not affect the value returned.

If the properties are expressed in the form of attribute values of the
Description element (property attributes), a temporary property element
will be created and returned."
  (when prop-ename-list
    (let (result)
      (xmp-desc-some-property-elements
       desc
       (lambda (_prop-ename place elem attr)
         (cond
          ((eq place 'attribute)
           ;; Convert attribute to element
           (push (xmp-property-element-from-attr attr) result))
          ((eq place 'element)
           (push elem result)))
         ;; Continue
         nil)
       prop-ename-list)
      (nreverse result))))

(defun xmp-desc-some-property-elements (desc predicate prop-ename-list)
  "Call PREDICATE for each XMP property that element DESC has, and when a
non-nil value is returned, exit and return that value.

PREDICATE is passed four arguments: PROP-ENAME, PLACE, ELEMENT, and
ATTRIBUTE.

PROP-ENAME is the property name (expanded name).

PLACE represents where the property resides and is either the symbol
`attribute' or the symbol `element'.

ELEMENT is the element in which the property resides. If PLACE is
`attribute', it is DESC. If PLACE is `element', it is the property
element that represents the property.

ATTRIBUTE is an attribute object that represents a property when PLACE
is `attribute', or nil when PLACE is `element'.

PROP-ENAME-LIST limits the properties to be processed. When it is the
symbol all, all properties are targeted. When it is a list of property
names, the properties included in the list are targeted."
  (or
   ;; Enumerate from the DESC's attributes
   ;; (simple, non-URI, unqualified values)
   ;; [XMP1 7.9.2.2]
   (let ((attrs (xmp-xml-element-attributes desc))
         result)
     (while (and attrs (null result))
       (let ((attr (pop attrs)))
         (when (and
                ;; Exclude nsdecl (xmlns= and xmlns:??=)
                (not (xmp-xml-attr-nsdecl-p attr))
                ;; Exclude non-property attributes
                ;; [XMP1 C.2.4]
                (not (member (xmp-xml-ename-ns (xmp-xml-attr-ename attr))
                             ;; rdf:*, xml:* : Maybe excessive?
                             ;; xmlns:* is namespace declaration
                             (list xmp-rdf: xmp-xml:)))
                ;; Filter by prop-ename-list
                (or (eq prop-ename-list 'all)
                    (xmp-xml-ename-member (xmp-xml-attr-ename attr)
                                          prop-ename-list)))
           (setq result (funcall predicate
                                 (xmp-xml-attr-ename attr)
                                 'attribute desc attr)))))
     result)

   ;; Enumerate from the DESC's children
   ;; [XMP1 C.2.5]
   (let ((children (xmp-xml-element-children desc))
         result)
     (while (and children (null result))
       (let ((child (pop children)))
         (when (and
                (xmp-xml-element-p child)
                ;; Filter by prop-ename-list
                (or (eq prop-ename-list 'all)
                    (xmp-xml-ename-member (xmp-xml-element-ename child)
                                          prop-ename-list)))
           (setq result (funcall predicate
                                 (xmp-xml-element-ename child)
                                 'element child nil)))))
     result)))

(defun xmp-desc-get-property-element (desc prop-ename)
  (or
   ;; Get from the DESC's attributes
   ;; (simple, non-URI, unqualified values)
   ;; [XMP1 7.9.2.2]
   (when-let ((attr (xmp-xml-element-attr-find-by-ename
                     desc prop-ename)))
     (xmp-property-element-from-attr attr))
   ;; Get from the DESC's children
   ;; [XMP1 C.2.5]
   (xmp-xml-element-child-find-by-ename desc prop-ename)))

(defun xmp-desc-remove-property (desc prop-ename)
  "Remove the property named PROP-ENAME from the Description element DESC.

The property will be removed whether it is expressed as an attribute or
a child element."
  (xmp-xml-element-attr-remove-by-ename desc prop-ename)
  (xmp-xml-element-child-remove-by-ename desc prop-ename))

(defun xmp-desc-set-property-element (desc prop-elem)
  "Add the property element PROP-ELEM to the Description element DESC.

Any existing property (whether attribute or element) with the same
property name will be deleted."

  (xmp-desc-remove-property desc (xmp-xml-element-ename prop-elem))
  (xmp-xml-element-insert-last desc prop-elem))

(defun xmp-desc-set-property-value (desc prop-ename value)
  "Add a property element to the Description element DESC.

The property element to be added is created from PROP-ENAME and VALUE
using the `xmp-property-element-from' function.

Any existing property (whether attribute or element) with the same
property name will be deleted."
  (xmp-desc-set-property-element desc
                                 (xmp-property-element-from prop-ename value)))



;;;; Parse Property Element (Property Element to PValue)

;; Property elements appear as children of nodeElement (Description,
;; array, and typed nodes).
;;
;;   <rdf:Description> <ns:Prop>(<-propertyElt)
;;   <rdf:Description> <rdf:value>(<-propertyElt)
;;   <rdf:Description> <ns:Qual>(<-propertyElt)
;;   <rdf:Description> <ns:Field>(<-propertyElt)
;;   <rdf:Bag> <rdf:li>(<-propertyElt)
;;   <rdf:Seq> <rdf:li>(<-propertyElt)
;;   <rdf:Alt> <rdf:li>(<-propertyElt)
;;   <ns:Type> propertyElt...

;; [XMP1 C.2.5] propertyElt ::
;; - resourcePropertyElt : Struct, Array, Qualified
;; - literalPropertyElt : Simple
;; - parseTypeResourcePropertyElt : <ns:Prop parseType="Resource">
;; - parseTypeLiteralPropertyElt : Not allowed
;; - parseTypeCollectionPropertyElt : Not allowed
;; - parseTypeOtherPropertyElt : Not allowed
;; - emptyPropertyElt : Simple, Struct

(defun xmp-property-element-type (elem)
  ;; [XMP1 C.2.5]
  (cond
   ;; If there are more than three attributes (counting xml:lang),
   ;; this is an emptyPropertyElt.
   ((>= (xmp-xml-element-attributes-count-without-nsdecl elem) 3)
    'empty)
   ;; Look for an attribute that is not xml:lang or rdf:ID.
   ;; If none is found, look at the XML content of the propertyElt.
   ((not (seq-find (lambda (attr)
                     (not (or (xmp-xml-attr-nsdecl-p attr)
                              (xmp-xml-attr-enamed-p attr xmp-xml:lang)
                              (xmp-xml-attr-enamed-p attr xmp-rdf:ID))))
                   (xmp-xml-element-attributes elem)))
    (cond
     ;; If there is no content, this is an emptyPropertyElt.
     ((null (xmp-xml-element-children elem))
      'empty)
     ;; If the only content is character data, this is a literalPropertyElt.
     ((seq-every-p #'xmp-xml-text-node-p (xmp-xml-element-children elem))
      'literal)
     ;; Otherwise this is a resourcePropertyElt.
     (t
      'resource)))
   ;; Otherwise (if an attribute is found that is not xml:lang or rdf:ID):
   (t
    (cond
     ;; If the attribute name is rdf:datatype, this is a literalPropertyElt.
     ((xmp-xml-element-attr-find-by-ename elem xmp-rdf:datatype)
      'literal)
     ((when-let ((parseType
                  (xmp-xml-element-attr-value elem xmp-rdf:parseType)))
        (cond
         ;; If the attribute value is Literal, this is a
         ;; parseTypeLiteralPropertyElt.
         ((equal parseType "Literal") 'parseTypeLiteral)
         ;; If the attribute value is Resource, this is a
         ;; parseTypeResourcePropertyElt.
         ((equal parseType "Resource") 'parseTypeResource)
         ;; If the attribute value is Collection, this is a
         ;; parseTypeCollectionPropertyElt.
         ((equal parseType "Collection") 'parseTypeCollection)
         ;; Otherwise, this is a parseTypeOtherPropertyElt.
         (t 'parseTypeOther))))
     ;; If the attribute name is not rdf:parseType, this is an
     ;; emptyPropertyElt.
     (t
      'empty)))))

(defun xmp-parse-property-element (prop-elem &optional noerror)
  "Parse the property element PROP-ELEM and return the parsed value (pvalue).

If NOERROR is non-nil, then nil is returned if an error occurs, whenever
possible."
  (if noerror
      (ignore-errors (xmp-parse-property-element--impl prop-elem))
    (xmp-parse-property-element--impl prop-elem)))

(defun xmp-parse-property-element--impl (prop-elem)
  (pcase (xmp-property-element-type prop-elem)
    ('empty
     (xmp-parse-property-element--empty prop-elem))
    ('resource
     (xmp-parse-property-element--resource prop-elem))
    ('literal
     (xmp-parse-property-element--literal prop-elem))
    ('parseTypeResource
     (xmp-parse-property-element--parse-type-resource prop-elem))
    (_ (error "Unsupported property element type"))))

(defun xmp-parse-property-element--resource (prop-elem)
  ;; [XMP1 C.2.6] resourcePropertyElt
  ;; <ns:Prop>
  ;;   nodeElement(Description|Bag|Seq|Alt|<typedNode>)
  ;; </>
  (let* ((prop-ename (xmp-xml-element-ename prop-elem))
         (xml-lang (xmp-xml-element-attr-value prop-elem xmp-xml:lang))
         (init-qualifiers (when xml-lang
                            (list (xmp-pvalue-make-named xmp-xml:lang 'text
                                                         xml-lang))))
         (node-elem (car (xmp-xml-element-children prop-elem))))
    (cond
     ;; Struct [XMP1 7.6] or Qualified Value [XMP1 7.8]
     ;; <ns:Prop>
     ;;   <rdf:Description>
     ;;     field... </></>
     ((xmp-xml-element-enamed-p node-elem xmp-rdf:Description)
      ;; parse fields (propertyEltList)
      (let* ((fields (mapcar #'xmp-parse-property-element
                             (xmp-xml-element-children node-elem)))
             (value (xmp-xml-ename-assoc xmp-rdf:value fields)))
        (if value
            ;; <rdf:value> ... </>
            ;; <ns:Qual> ... </>...
            ;; TODO: Avoid dependence on pvalue structures.
            ;;       Create and use xmp-pvalue-* functions.
            (cons prop-ename
                  (plist-put (cdr value)
                             :qualifiers
                             (append
                              init-qualifiers
                              (seq-remove (lambda (field)
                                            (xmp-xml-ename-equal (car field)
                                                                 xmp-rdf:value))
                                          fields)
                              (xmp-pvalue-qualifier-alist (cdr value)))))
          ;; Struct
          (xmp-pvalue-make-named prop-ename 'struct fields init-qualifiers))))
     ;; Array [XMP1 7.7]
     ;; <ns:Prop><rdf:Bag> item... </></>
     ;; <ns:Prop><rdf:Seq> item... </></>
     ;; <ns:Prop><rdf:Alt> item... </></>
     ((or (xmp-xml-element-enamed-p node-elem xmp-rdf:Bag)
          (xmp-xml-element-enamed-p node-elem xmp-rdf:Seq)
          (xmp-xml-element-enamed-p node-elem xmp-rdf:Alt))
      (let ((items (cl-loop for child in (xmp-xml-element-children node-elem)
                            for field = (xmp-parse-property-element child)
                            when (xmp-xml-ename-equal (car field) xmp-rdf:li)
                            collect (cdr field))))
        (xmp-pvalue-make-named prop-ename 'array items init-qualifiers
                               ;; Bag | Seq | Alt
                               :array-type (xmp-xml-element-ename node-elem))))
     ;; Typed Node [XMP1 7.9.2.5]
     ;; <ns:Prop><ns:Type> ... </></>
     ;; => <ns:Prop><rdf:Description><rdf:type .../> ... </></>
     (t
      (let* ((node-elem-name (xmp-xml-element-ename node-elem))
             (type-name
              (concat
               (xmp-xml-ns-name-string (xmp-xml-ename-ns node-elem-name))
               (xmp-xml-ename-local node-elem-name))))
        (xmp-parse-property-element--modify-qualifier
         (xmp-parse-property-element--resource
          ;; <prop-ename>
          (xmp-xml-element
           prop-ename
           (xmp-xml-element-attributes prop-elem)
           ;; <rdf:Description>
           (list
            (xmp-xml-element
             xmp-rdf:Description
             nil
             ;; <rdf:type resource="<type-name>" />
             ;; ...
             (xmp-xml-element-children node-elem)))))
         ;; Add rdf:type qualifier
         xmp-rdf:type
         (xmp-pvalue-make 'uri type-name)))))))

(defun xmp-parse-property-element--literal (prop-elem)
  ;; [XMP1 C.2.7] literalPropertyElt
  ;; <ns:Prop>TEXT ONLY</>
  (let ((prop-ename (xmp-xml-element-ename prop-elem))
        (qualifiers (cl-loop for attr in (xmp-xml-element-attributes prop-elem)
                             unless (xmp-xml-attr-nsdecl-p attr)
                             collect (xmp-pvalue-make-named
                                      (xmp-xml-attr-ename attr)
                                      'text
                                      (xmp-xml-attr-value attr)))))
    (xmp-pvalue-make-named prop-ename 'text
                           (xmp-xml-element-children-text prop-elem)
                           qualifiers)))

(defun xmp-parse-property-element--parse-type-resource (prop-elem)
  ;; [XMP1 C.2.9] parseTypeResourcePropertyElt
  ;; <ns:Prop rdf:parseType="Resource"> ... </>
  ;; => <ns:Prop><rdf:Description> ... </></>
  (xmp-parse-property-element--resource
   ;; <prop-ename>
   (xmp-xml-element
    (xmp-xml-element-ename prop-elem)
    (seq-remove (lambda (attr)
                  (or (xmp-xml-attr-nsdecl-p attr)
                      (xmp-xml-attr-enamed-p attr xmp-rdf:parseType)))
                (xmp-xml-element-attributes prop-elem))
    ;; <rdf:Description>
    ;;   ...
    (list
     (xmp-xml-element xmp-rdf:Description
                      nil
                      (xmp-xml-element-children prop-elem))))))

(defun xmp-parse-property-element--empty (prop-elem)
  ;; According to the rule of [XMP1 C.2.5 Content of a nodeElement],
  ;; it may be judged as emptyPropertyElt even though there are child
  ;; nodes. It seems that such properties are ignored in XMP SDK.
  (when (xmp-xml-element-children prop-elem)
    (error "emptyPropertyElt has children"))

  ;; [XMP1 C.2.12]
  (cond
   ;; 1 If there is an rdf:value attribute, then this is a simple
   ;; property. All other attributes are qualifiers.
   ;; <ns:Prop3 rdf:value="..." ns:Qual="..."/>
   ((when-let ((value (xmp-xml-element-attr-value prop-elem xmp-rdf:value)))
      (xmp-pvalue-make-named
       (xmp-xml-element-ename prop-elem)
       'text
       value
       (cl-loop for attr in (xmp-xml-element-attributes prop-elem)
                unless (or (xmp-xml-attr-nsdecl-p attr)
                           (xmp-xml-attr-enamed-p attr xmp-rdf:value))
                collect (xmp-pvalue-from-attr attr)))))

   ;; 2 If there is an rdf:resource attribute, then this is a simple
   ;; property with a URI value. All other attributes are qualifiers.
   ;; <ns:Prop2 rdf:resource="http://www.adobe.com/"/>
   ((when-let ((value (xmp-xml-element-attr-value prop-elem xmp-rdf:resource)))
      (xmp-pvalue-make-named
       (xmp-xml-element-ename prop-elem)
       'uri
       value
       (cl-loop for attr in (xmp-xml-element-attributes prop-elem)
                unless (or (xmp-xml-attr-nsdecl-p attr)
                           (xmp-xml-attr-enamed-p attr xmp-rdf:resource))
                collect (xmp-pvalue-from-attr attr)))))
   ;; 3 If there are no attributes other than xml:lang, rdf:ID, or
   ;; rdf:nodeID, then this is a simple property with an empty value.
   ;; <ns:Prop1 [xml:lang=""] [rdf:ID=""] [rdf:nodeID=""] />
   ((not (seq-find (lambda (attr) (not (or (xmp-xml-attr-nsdecl-p attr)
                                           (xmp-xml-ename-member
                                            (xmp-xml-attr-ename attr)
                                            xmp-names:lang-ID-nodeID))))
                   (xmp-xml-element-attributes prop-elem)))
    ;; TODO: It's unclear how to handle empty values; the SDK treats
    ;; them as if the property doesn't exist.
    (xmp-pvalue-make-named
     (xmp-xml-element-ename prop-elem)
     'text
     ""
     (when-let ((xml-lang (xmp-xml-element-attr-value prop-elem xmp-xml:lang)))
       (list (xmp-pvalue-make-named xmp-xml:lang 'text xml-lang)))))
   ;; 4 Finally, this is a struct, and the attributes other than
   ;; xml:lang, rdf:ID, or rdf:nodeID are the fields.
   ;; <ns:Prop4 ns:Field1="..." ns:Field2="..."/>
   (t
    (xmp-pvalue-make-named
     (xmp-xml-element-ename prop-elem)
     'struct
     (cl-loop for attr in (xmp-xml-element-attributes prop-elem)
              unless (or (xmp-xml-attr-nsdecl-p attr)
                         ;; xml:lang rdf:ID rdf:nodeID
                         (xmp-xml-ename-member (xmp-xml-attr-ename attr)
                                               xmp-names:lang-ID-nodeID))
              collect (xmp-pvalue-from-attr attr))
     (when-let ((xml-lang (xmp-xml-element-attr-value prop-elem xmp-xml:lang)))
       (list (xmp-pvalue-make-named xmp-xml:lang 'text xml-lang)))))))

(defun xmp-parse-property-element--modify-qualifier (prop-name-value
                                                     qname
                                                     qvalue)
  (when (consp prop-name-value)
    (setf (cdr prop-name-value)
          (xmp-pvalue-qualifier-set (cdr prop-name-value) qname qvalue)))
  prop-name-value)

;;;; Parsed Value (PValue)

;;;;; PValue Creation

(defun xmp-pvalue-make-named (name type value &optional qualifiers &rest props)
  (cons name
        (apply #'xmp-pvalue-make type value qualifiers
               props)))

(defun xmp-pvalue-make (type value &optional qualifiers &rest props)
  (nconc
   (list :pv-type type :value value)
   (when qualifiers (list :qualifiers qualifiers))
   props))

(defun xmp-pvalue-from-attr (attr)
  (xmp-pvalue-make-named (xmp-xml-attr-ename attr)
                         'text
                         (xmp-xml-attr-value attr)))

(defun xmp-pvalue-from (value)
  (cond
   ((stringp value)
    (xmp-pvalue-make-text value))
   ((xmp-pvalue-maybe-p value)
    value)
   (t
    (error "Unsupported value form"))))

;;;;; PValue Predicate

(defun xmp-pvalue-maybe-p (pvalue)
  (and (consp pvalue)
       (keywordp (car pvalue))
       (plist-get pvalue :pv-type)))

(defun xmp-pvalue-pure-p (pvalue)
  "Return t if PVALUE has only a value.

If PVALUE has only type information and a value, and no other
infomation (such as qualifiers), return t.
Such PVALUEs can be edited with a relatively simple UI."
  (and (xmp-pvalue-maybe-p pvalue)
       (null (xmp-pvalue-qualifier-alist pvalue))))

(defun xmp-pvalue-text-p (pvalue)
  (and (consp pvalue) (eq (xmp-pvalue-type pvalue) 'text)))

(defun xmp-pvalue-uri-p (pvalue)
  (and (consp pvalue) (eq (xmp-pvalue-type pvalue) 'uri)))

(defun xmp-pvalue-array-p (pvalue)
  (and (consp pvalue) (eq (xmp-pvalue-type pvalue) 'array)))

(defun xmp-pvalue-struct-p (pvalue)
  (and (consp pvalue) (eq (xmp-pvalue-type pvalue) 'struct)))

;;;;; PValue Accessor

(defun xmp-pvalue-type (pvalue)
  "Return the type of the PVALUE.

The type is one of the following symbols:
- text
- uri
- array
- struct"
  (plist-get pvalue :pv-type))

(defun xmp-pvalue-value (pvalue)
  "Return the value of PVALUE.

The format of the value depends on the type of PVALUE.

- text : String
- uri : String
- array : List of pvalues
- struct : List of (ename . pvalue)

The type of PVALUE can be obtained by `xmp-pvalue-type'.
The type of the array can be obtained by `xmp-pvalue-array-type'."
  (plist-get pvalue :value))

(defun xmp-pvalue-array-type (pvalue)
  "Return the array type of PVALUE.

The type of array is one of the following variable values (expanded names):
- xmp-rdf:Bag
- xmp-rdf:Seq
- xmp-rdf:Alt"
  (plist-get pvalue :array-type))

(defun xmp-pvalue-qualifier-alist (pvalue)
  "Return the qualifier alist of PVALUE."
  (plist-get pvalue :qualifiers))

(defun xmp-pvalue-qualifier-get (pvalue ename)
  "Return the pvalue of the qualifier specified by the ENAME of PVALUE."
  (xmp-xml-ename-alist-get ename (xmp-pvalue-qualifier-alist pvalue)))

(defun xmp-pvalue-qualifier-alist-without-xml:lang (pvalue)
  "Return the qualifier alist of PVALUE excluding xml:lang."
  (seq-remove (lambda (parsed-qual)
                (xmp-xml-ename-equal (car parsed-qual) xmp-xml:lang))
              (xmp-pvalue-qualifier-alist pvalue)))

(defun xmp-pvalue-xml:lang (pvalue)
  "Return the xml:lang qualifier string of PVALUE."
  (xmp-pvalue-as-text
   (xmp-pvalue-qualifier-get pvalue xmp-xml:lang)))

;;;;; PValue Modification

(defun xmp-pvalue-qualifier-set (pvalue qname qvalue)
  (let ((qlink (plist-member pvalue :qualifiers)))
    (unless qlink
      (setq pvalue
            (nconc
             pvalue
             (setq qlink (list :qualifiers nil)))))
    (setf (alist-get qname (cadr qlink) nil nil #'xmp-xml-ename-equal)
          qvalue))
  pvalue)

;;;;; PValue Conversion
;;;;;; Convert By Type Name

(defconst xmp-pvalue-types
  '((Text xmp-pvalue-as-text xmp-pvalue-make-text)
    (URI xmp-pvalue-as-uri xmp-pvalue-make-uri)
    (Boolean xmp-pvalue-as-boolean xmp-pvalue-make-boolean)
    (Real xmp-pvalue-as-real xmp-pvalue-make-real)
    (MIMEType xmp-pvalue-as-text xmp-pvalue-make-text)
    (AgentName xmp-pvalue-as-text xmp-pvalue-make-text)
    (LangAlt xmp-pvalue-as-lang-alt-alist xmp-pvalue-from-lang-alt-alist)
    (BagText xmp-pvalue-as-text-list xmp-pvalue-make-bag-from-text-list)
    (BagProperName xmp-pvalue-as-text-list xmp-pvalue-make-bag-from-text-list)
    (BagLocale xmp-pvalue-as-text-list xmp-pvalue-make-bag-from-text-list)
    (SeqText xmp-pvalue-as-text-list xmp-pvalue-make-seq-from-text-list)
    (SeqProperName xmp-pvalue-as-text-list xmp-pvalue-make-seq-from-text-list)
    (SeqLocale xmp-pvalue-as-text-list xmp-pvalue-make-seq-from-text-list)
    ;;(SeqDate xmp-pvalue-as-text-list xmp-pvalue-make-seq-from-text-list)
    ;; GUID
    ;; Date
    ;; Integer
    ;; RenditionClass
    ;; ResourceRef
    ))

(defun xmp-pvalue-make-by-type (type value)
  (when-let* ((type-info (assq type xmp-pvalue-types))
              (encoder (nth 2 type-info)))
    (funcall encoder value)))
;; TEST: (xmp-pvalue-make-by-type 'Real -1) => (:pv-type text :value "-1")

(defun xmp-pvalue-as-type (type pvalue)
  (when-let* ((type-info (assq type xmp-pvalue-types))
              (decoder (nth 1 type-info)))
    (funcall decoder pvalue)))
;; TEST: (xmp-pvalue-as-type 'Real (xmp-pvalue-make-real 5)) => 5

;;;;;; Convert Predefined Properties

(defun xmp-defined-property-pvalue-from-elisp (prop-ename value)
  (when-let ((type (xmp-defined-property-type prop-ename)))
    (xmp-pvalue-make-by-type type value)))

(defun xmp-defined-property-pvalue-to-elisp (prop-ename pvalue)
  (when-let ((type (xmp-defined-property-type prop-ename)))
    (xmp-pvalue-as-type type pvalue)))

;;;;;; Text Type

(defun xmp-pvalue-make-text (text &optional qualifiers)
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (xmp-pvalue-make 'text text qualifiers))

(defun xmp-pvalue-as-text (pvalue)
  "Convert PVALUE to a string.
If the type of PVALUE is not \\='text, return nil."
  (when (xmp-pvalue-text-p pvalue)
    (xmp-pvalue-value pvalue)))

;;;;;; URI Type

(defun xmp-pvalue-make-uri (uri &optional qualifiers)
  (unless (stringp uri)
    (signal 'wrong-type-argument (list 'stringp uri)))
  (xmp-pvalue-make 'uri uri qualifiers))

(defun xmp-pvalue-as-uri (pvalue)
  "Convert PVALUE to a string.
If the type of PVALUE is not \\='uri, return nil."
  (when (xmp-pvalue-uri-p pvalue)
    (xmp-pvalue-value pvalue)))

;;;;;; Array Type

(defun xmp-pvalue-make-bag (pvalue-list &optional qualifiers)
  (xmp-pvalue-make 'array (mapcar #'xmp-pvalue-from pvalue-list)
                   qualifiers :array-type xmp-rdf:Bag))

(defun xmp-pvalue-make-seq (pvalue-list &optional qualifiers)
  (xmp-pvalue-make 'array (mapcar #'xmp-pvalue-from pvalue-list)
                   qualifiers :array-type xmp-rdf:Seq))

(defun xmp-pvalue-make-alt (pvalue-list &optional qualifiers)
  (xmp-pvalue-make 'array (mapcar #'xmp-pvalue-from pvalue-list)
                   qualifiers :array-type xmp-rdf:Alt))

(defun xmp-pvalue-as-list (pvalue)
  "Convert PVALUE to a list of pvalues.
If the type of PVALUE is not \\='array, return nil."
  (when (xmp-pvalue-array-p pvalue)
    (xmp-pvalue-value pvalue)))

;;;;;; Struct Type

(defun xmp-pvalue-make-struct (ename-pvalue-alist &optional qualifiers)
  (xmp-pvalue-make 'struct
                   (mapcar (lambda (ename-pvalue)
                             (cons (car ename-pvalue)
                                   (xmp-pvalue-from (cdr ename-pvalue))))
                           ename-pvalue-alist)
                   qualifiers))

(defun xmp-pvalue-as-struct (pvalue)
  "Convert PVALUE to a alist of enames and pvalues.
If the type of PVALUE is not \\='struct, return nil."
  (when (xmp-pvalue-struct-p pvalue)
    (xmp-pvalue-value pvalue)))

;;;;;; Boolean
;; [XMP1 8.2.1.1 Boolean]

(defun xmp-pvalue-make-boolean (nil-or-nonnil)
  (xmp-pvalue-make-text (if nil-or-nonnil "True" "False")))
;; TEST: (xmp-pvalue-make-boolean nil) => (:pv-type text :value "False")
;; TEST: (xmp-pvalue-make-boolean t) => (:pv-type text :value "True")
;; TEST: (xmp-pvalue-make-boolean 123) => (:pv-type text :value "True")

(defun xmp-pvalue-as-boolean (pvalue &optional default)
  (pcase (xmp-pvalue-as-text pvalue)
    ("True" t)
    ("False" nil)
    (_ default)))
;; TEST: (xmp-pvalue-as-boolean (xmp-pvalue-make-text "True")) => t
;; TEST: (xmp-pvalue-as-boolean (xmp-pvalue-make-text "False")) => nil
;; TEST: (xmp-pvalue-as-boolean (xmp-pvalue-make-text "true") 'unknown) => unknown

;;;;;; Date
;; [XMP1 8.2.1.2 Date]

;; TODO: implement Date type conversion

;;;;;; Integer
;; [XMP1 8.2.1.3 Integer]

;; TODO: implement Integer type conversion

;;;;;; Real
;; [XMP1 8.2.1.4 Real]

(defun xmp-pvalue-make-real (number)
  (xmp-pvalue-make-text (number-to-string number)))

(defun xmp-pvalue-as-real (pvalue)
  (when-let ((text (xmp-pvalue-as-text pvalue)))
    (string-to-number text)))

;;;;;; Language Alternative
;; [XMP1 8.2.2.4 Language Alternative]

(defun xmp-pvalue-as-lang-alt-alist (pvalue)
  (if-let ((text (xmp-pvalue-as-text pvalue)))
      (list (cons "x-default" text))
    (cl-loop for item in (xmp-pvalue-as-list pvalue)
             for lang = (xmp-pvalue-as-text
                         (xmp-pvalue-qualifier-get item xmp-xml:lang))
             for text = (xmp-pvalue-as-text item)
             when text
             collect (cons lang text))))

(defun xmp-pvalue-from-lang-alt-alist (lang-alt-alist)
  "LANG-ALT-ALIST is an alist whose keys are strings or language code
strings and whose values are strings. When specifying an alist, the
first language code must be \"x-default\"."
  (when (stringp lang-alt-alist)
    (setq lang-alt-alist (list (cons "x-default" lang-alt-alist))))
  (xmp-pvalue-make-alt
   (mapcar (lambda (item)
             (xmp-pvalue-make-text (cdr item)
                                   (list
                                    (xmp-pvalue-make-named xmp-xml:lang
                                                           'text
                                                           (car item)))))
           lang-alt-alist)))

(defun xmp-lang-alt-alist-to-single-string (lang-alt-alist)
  (if (cdr lang-alt-alist)
      (mapconcat (lambda (item) (format "[%s]:%s" (car item) (cdr item)))
                 lang-alt-alist "; ")
    (cdar lang-alt-alist)))

;;;;;; Array of Text

(defun xmp-pvalue-make-bag-from-text-list (text-list)
  (xmp-pvalue-make-bag
   (cl-loop for text in text-list
            when (stringp text)
            collect (xmp-pvalue-make-text text))))

(defun xmp-pvalue-make-seq-from-text-list (text-list)
  (xmp-pvalue-make-seq
   (cl-loop for text in text-list
            when (stringp text)
            collect (xmp-pvalue-make-text text))))

(defun xmp-pvalue-as-text-list (pvalue)
  (cl-loop for item in (xmp-pvalue-as-list pvalue)
           for text = (xmp-pvalue-as-text item)
           when text
           collect text))

;;;;; PValue Dump

(defun xmp-dump-indent (stream indent)
  (princ (make-string (* 2 indent) ? ) stream))

(defun xmp-dump-ename (stream ename ns-name-prefix-alist)
  (princ (xmp-xml-ename-string ename ns-name-prefix-alist 'uri) stream))

(defun xmp-dump-pvalue (stream pvalue ns-name-prefix-alist indent)
  (let* ((qualifiers (xmp-pvalue-qualifier-alist pvalue))
         (lang (xmp-pvalue-as-text
                (xmp-xml-ename-alist-get xmp-xml:lang qualifiers))))
    (when lang
      (princ (format "[lang:%s] " lang) stream)
      (setf (xmp-xml-ename-alist-get xmp-xml:lang qualifiers nil t) nil))

    (pcase (xmp-pvalue-type pvalue)
      ('text (princ (format "%s\n" (xmp-pvalue-value pvalue)) stream))
      ('uri (princ (format "(uri)%s\n" (xmp-pvalue-value pvalue)) stream))
      ('array
       (princ (format "(%s array)\n"
                      (xmp-xml-ename-local (xmp-pvalue-array-type pvalue)))
              stream)
       (dolist (item (xmp-pvalue-value pvalue))
         (xmp-dump-indent stream (1+ indent))
         (princ "- " stream)
         (xmp-dump-pvalue stream item ns-name-prefix-alist (1+ indent))))
      ('struct
       (princ "(struct)\n" stream)
       (xmp-dump-named-pvalue-list stream (xmp-pvalue-value pvalue)
                                   ns-name-prefix-alist (1+ indent)))
      (_
       (princ (format "(unknown)%s\n" pvalue) stream)))

    (when qualifiers
      (xmp-dump-indent stream (1+ indent))
      (princ "qualifiers:\n" stream)
      (xmp-dump-named-pvalue-list stream qualifiers
                                  ns-name-prefix-alist (+ 2 indent)))))

(defun xmp-dump-named-pvalue (stream name-pvalue ns-name-prefix-alist indent)
  (let ((name (car name-pvalue))
        (pvalue (cdr name-pvalue)))
    (xmp-dump-ename stream name ns-name-prefix-alist)
    (princ " : " stream)
    (xmp-dump-pvalue stream pvalue ns-name-prefix-alist indent)))

(defun xmp-dump-named-pvalue-list (stream named-pvalue-list
                                          ns-name-prefix-alist indent)
  (dolist (named-pvalue named-pvalue-list)
    (xmp-dump-indent stream indent)
    (xmp-dump-named-pvalue stream named-pvalue
                           ns-name-prefix-alist indent)))

(defun xmp-dump-ns-name-prefix-alist (stream ns-name-prefix-alist)
  (cl-loop for (ns-name . ns-prefix) in ns-name-prefix-alist
           do (princ (format "%s = %s\n"
                             ns-prefix
                             (xmp-xml-ns-name-string ns-name))
                     stream)))

(defun xmp-dump-used-ns-name-prefix (stream named-pvalue-list
                                            ns-name-prefix-alist)
  (let ((used-ns-names (xmp-pvalue-used-ns-names--from-named-pvalue-list
                        named-pvalue-list nil)))
    (xmp-dump-ns-name-prefix-alist
     stream
     (cl-loop for ns-name in (nreverse used-ns-names)
              ;; TODO: Unify xmp-xml-ename-string?
              for prefix = (or (when (equal ns-name xmp-xml:) "xml")
                               (xmp-xml-default-ns-prefix ns-name)
                               (alist-get ns-name ns-name-prefix-alist))
              when prefix
              collect (cons ns-name prefix)))))

;; Collect used namespace from pvalue

(defun xmp-pvalue-used-ns-names--from-named-pvalue (named-pvalue ns-names)
  (when-let ((ns-name (xmp-xml-ename-ns (car named-pvalue))))
    (unless (member ns-name ns-names)
      (push ns-name ns-names)))
  (setq ns-names
        (xmp-pvalue-used-ns-names--from-pvalue (cdr named-pvalue) ns-names))
  ns-names)

(defun xmp-pvalue-used-ns-names--from-named-pvalue-list (pvalue-list ns-names)
  (dolist (named-pvalue pvalue-list)
    (setq ns-names
          (xmp-pvalue-used-ns-names--from-named-pvalue named-pvalue ns-names)))
  ns-names)

(defun xmp-pvalue-used-ns-names--from-pvalue (pvalue ns-names)
  ;; From value
  (pcase (xmp-pvalue-type pvalue)
    ;; Skip text, uri
    ('array
     (dolist (item (xmp-pvalue-value pvalue))
       (setq ns-names (xmp-pvalue-used-ns-names--from-pvalue
                       item ns-names))))
    ('struct
     (setq ns-names (xmp-pvalue-used-ns-names--from-named-pvalue-list
                     (xmp-pvalue-value pvalue) ns-names))))
  ;; From qualifiers
  (when-let ((qualifiers (xmp-pvalue-qualifier-alist pvalue)))
    (setq ns-names (xmp-pvalue-used-ns-names--from-named-pvalue-list
                    qualifiers ns-names)))
  ns-names)

;;;; Generate Property Element
;;;;; Property Element from Parsed Value

(defun xmp-property-element-from-named-pvalue (named-pvalue)
  (xmp-property-element-from-pvalue (car named-pvalue) (cdr named-pvalue)))

(defun xmp-property-element-from-pvalue (prop-ename pvalue)
  (pcase (xmp-pvalue-type pvalue)
    ('text
     (xmp-property-element-from-parsed-text prop-ename pvalue))
    ('uri
     (xmp-property-element-from-parsed-uri prop-ename pvalue))
    ('struct
     (xmp-property-element-from-parsed-struct prop-ename pvalue))
    ('array
     (xmp-property-element-from-parsed-array prop-ename pvalue))
    (_
     (error "Unsupported value type"))))

(defun xmp-property-element-from-parsed-text (prop-ename pvalue)
  ;; [XMP1 7.5 Simple valued XMP properties]
  (xmp-property-element--make prop-ename pvalue nil
                              ;; Text
                              (list (xmp-pvalue-value pvalue))))

(defun xmp-property-element-from-parsed-uri (prop-ename pvalue)
  ;; [XMP1 7.5 Simple valued XMP properties]
  (xmp-property-element--make prop-ename pvalue
                              ;; rdf:resource="[URI]"
                              (list (xmp-xml-attr xmp-rdf:resource
                                                  (xmp-pvalue-value pvalue)))
                              nil))

(defun xmp-property-element-from-parsed-struct (prop-ename pvalue)
  ;; [XMP1 7.6 Structure valued XMP properties]
  (xmp-property-element--make
   prop-ename pvalue
   nil
   (list
    ;; <rdf:Description>
    ;;   [fields]
    ;; </rdf:Description>
    (xmp-xml-element xmp-rdf:Description
                     nil
                     (mapcar #'xmp-property-element-from-named-pvalue
                             (xmp-pvalue-value pvalue))))))

(defun xmp-property-element-from-parsed-array (prop-ename pvalue)
  ;; [XMP1 7.7 Array valued XMP properties]
  (xmp-property-element--make
   prop-ename pvalue
   nil
   (list
    ;; <rdf:Bag> (Bag | Seq | Alt)
    ;;   <rdf:li> [item] </rdf:li>
    ;;   <rdf:li> [item] </rdf:li>
    ;;   <rdf:li> [item] </rdf:li>
    ;; </rdf:Bag>
    (xmp-xml-element
     (xmp-pvalue-array-type pvalue) ;; xmp-rdf:Bag | xmp-rdf:Seq | xmp-rdf:Alt
     nil
     (cl-loop for item in (xmp-pvalue-value pvalue)
              collect (xmp-property-element-from-pvalue xmp-rdf:li item))))))

(defun xmp-property-element--make (prop-ename
                                   pvalue value-attributes value-children)
  ;; [XMP1 7.5 Simple valued XMP properties]
  ;; [XMP1 7.6 Structure valued XMP properties]
  ;; [XMP1 7.7 Array valued XMP properties]
  ;; [XMP1 7.8 Qualifiers]
  ;; [XMP1 C.2.6 The resourcePropertyElt]
  ;; [XMP1 C.2.7 The literalPropertyElt]
  ;; [XMP1 C.2.12 The emptyPropertyElt]
  (let ((xml-lang (xmp-pvalue-xml:lang pvalue))
        (qualifiers (xmp-pvalue-qualifier-alist-without-xml:lang pvalue)))
    ;; TODO: Use attribute qualifier notation ? (If all qualifiers are text)
    ;;       <[prop-ename] rdf:value="[value-text]" [qualifiers] />
    (if qualifiers
        ;; <[prop-ename] xml:lang=[xml-lang]>
        ;;   <rdf:Description>
        ;;     <rdf:value [value-attributes]>[value-children]</rdf:value>
        ;;     [qualifiers]
        ;;   </rdf:Description>
        ;; </[prop-ename]>
        (xmp-xml-element
         prop-ename
         (when xml-lang (list (xmp-xml-attr xmp-xml:lang xml-lang)))
         (list
          (xmp-xml-element
           xmp-rdf:Description
           nil
           (cons
            (xmp-xml-element xmp-rdf:value value-attributes value-children)
            (mapcar #'xmp-property-element-from-named-pvalue
                    qualifiers)))))
      ;; <[prop-ename] xml:lang=[xml-lang] [value-attributes]>
      ;;   [value-children]
      ;; </[prop-ename]>
      (xmp-xml-element
       prop-ename
       (nconc
        (when xml-lang (list (xmp-xml-attr xmp-xml:lang xml-lang)))
        value-attributes)
       value-children))))


;;;;; Property Element from Lisp Value

(defun xmp-property-element-from (prop-ename value)
  (cond
   ((stringp value)
    (xmp-property-element-from-parsed-text prop-ename (xmp-pvalue-make
                                                       'text value)))
   ((xmp-pvalue-maybe-p value)
    (xmp-property-element-from-pvalue prop-ename value))
   (t
    (error "Unsupported value form"))))

;;;;; Property Element from Attribute

(defun xmp-property-element-from-attr (attr)
  (xmp-xml-element (xmp-xml-attr-ename attr)
                   nil
                   (list (xmp-xml-attr-value attr))))


;;;; File I/O
;;;;; XMP Packet Wrapper

(defun xmp-scan-xmp-packet-region (beg end)
  (save-excursion
    (goto-char beg)
    ;; [XMP1 7.3.2 XMP packet wrapper]
    (when (re-search-forward
           "<\\?xpacket[ \t\n\r]+begin=\\([\"']\\)\\(?:\xef\xbb\xbf\\)?\\1[ \t\n\r]+id=\\([\"']\\)W5M0MpCehiHzreSzNTczkc9d\\2.*?\\?>" end t)
      (let ((header-beg (match-beginning 0))
            (header-end (match-end 0)))
        (when (re-search-forward
               "<\\?xpacket[ \t\n\r]+end=\\([\"']\\)\\([rw]\\)\\1.*?\\?>" end t)
          (let ((trailer-beg (match-beginning 0))
                (trailer-end (match-end 0))
                (writable (string= (match-string 2) "w")))
            (list
             :bytes (buffer-substring header-end trailer-beg)
             :header (buffer-substring header-beg header-end)
             :trailer (buffer-substring trailer-beg trailer-end)
             :header-begin header-beg :header-end header-end
             :trailer-begin trailer-beg :trailer-end trailer-end
             :writable writable)))))))

(defun xmp-file-scan-xmp-packet (file &optional beg end)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil beg end)
    (xmp-scan-xmp-packet-region (point-min) (point-max))))

(defcustom xmp-file-read-packet-scan-size (* 64 1024 1024)
  "The number of bytes from the beginning of the file to scan for XMP packets.

nil means to scan the entire file."
  :group 'xmp
  :type '(choice integer (const nil)))

(defun xmp-file-read-xml-from-scanned-packet (file)
  ;; Remove all xmlns:??= attributes that exist on non-root elements.
  ;; When the `xmp-xml-element-attributes' returns namespace
  ;; declarations, there are a few places where it will not work
  ;; correctly.
  (when-let ((packet-body (plist-get (xmp-file-scan-xmp-packet
                                      file
                                      (when xmp-file-read-packet-scan-size 0)
                                      xmp-file-read-packet-scan-size)
                                     :bytes)))
    (xmp-xml-move-nsdecls-to-root
     (xmp-xml-parse-string
      (decode-coding-string packet-body 'utf-8)))))

(defun xmp-file-write-xml-to-scanned-packet (file dom)
  (let* ((xml-bytes
          (with-temp-buffer
            (let ((xmp-xml-no-line-break t))
              (xmp-xml-print (current-buffer) dom
                             xmp-xml-default-ns-name-prefix-alist t))
            (encode-coding-region (point-min) (point-max) 'utf-8 t)))
         (xml-size (length xml-bytes)))

    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (let ((packet-info (xmp-scan-xmp-packet-region (point-min) (point-max))))
        (unless packet-info
          (error "No XMP packet wrapper in file"))
        (unless (plist-get packet-info :writable)
          (error "XMP packet is read-only"))
        (let* ((body-beg (plist-get packet-info :header-end))
               (body-end (plist-get packet-info :trailer-begin))
               (body-size (- body-end body-beg)))
          (when (> xml-size body-size)
            (error "XMP packet size is too small (packet:%d xml:%d)"
                   body-size xml-size))
          ;; Replace packet body
          (goto-char body-beg)
          (delete-region body-beg body-end)
          (insert xml-bytes)
          (insert (make-string (- body-size xml-size) ? )))))))

;;;;; JPEG File

(defcustom xmp-jpeg-extract-exif-p t
  "If non-nil, EXIF information in JPEG files will be converted to XMP."
  :type 'boolean
  :group 'xmp)

(defconst xmp-file-scan-jpeg-exif-signature
  "Exif\0\0")
(defconst xmp-file-scan-jpeg-exif-signature-info
  `(,(regexp-quote xmp-file-scan-jpeg-exif-signature)
    ,(length xmp-file-scan-jpeg-exif-signature)
    exif
    nil))

(defconst xmp-file-scan-jpeg-xmp-signature
  "http://ns.adobe.com/xap/1.0/\0") ;;[XMP3 1.1.3 JPEG]
(defconst xmp-file-scan-jpeg-xmp-signature-size
  (length xmp-file-scan-jpeg-xmp-signature))
(defconst xmp-file-scan-jpeg-xmp-signature-info
  `(,(regexp-quote xmp-file-scan-jpeg-xmp-signature)
    ,(length xmp-file-scan-jpeg-xmp-signature)
    standard-xmp
    nil))

(defconst xmp-file-scan-jpeg-xmp-ext-signature
  "http://ns.adobe.com/xmp/extension/\0") ;; [XMP3 1.1.3.1 Extended XMP in JPEG]
(defconst xmp-file-scan-jpeg-xmp-ext-signature-size
  (length "http://ns.adobe.com/xmp/extension/\0"))
(defconst xmp-file-scan-jpeg-xmp-ext-signature-info
  `(,(regexp-quote xmp-file-scan-jpeg-xmp-ext-signature)
    ,(length xmp-file-scan-jpeg-xmp-ext-signature)
    extended-xmp
    t)) ;; multiple segments

(defun xmp-file-scan-jpeg-app1 (file app1-signature-info-list)
  (setq app1-signature-info-list (copy-sequence app1-signature-info-list))
  (with-temp-buffer
    (let ((reader (xmp-file-reader-open file))
          result)
      (unless (= (xmp-file-reader-u16be reader) #xffd8) ;; ffd8:Start Of Image
        (error "Not a JPEG file"))
      (while
          (and
           app1-signature-info-list
           (let ((marker (xmp-file-reader-u16be reader)))
             ;;(message "marker=%x" marker)
             (unless (or (= marker #xffd9) ;; ffd9:End Of Image
                         (= marker #xffda)) ;; ffda:Start Of Scan
               ;; ^(Normally, it would be enough in front of SOF. [XMP3 1.1.3])
               (let ((segoff (xmp-file-reader-current-offset reader))
                     (seglen (xmp-file-reader-u16be reader)))
                 ;;(message "segoff=%d seglen=%d" segoff seglen)
                 (when (= marker #xffe1) ;; ffe1:APP1
                   (xmp-file-reader-ensure-bytes
                    reader
                    (cl-loop for sig-info in app1-signature-info-list
                             maximize (nth 1 sig-info))
                    'noerror)
                   (when-let ((sig-info (seq-find
                                         (lambda (sig-info)
                                           (looking-at-p (nth 0 sig-info)))
                                         app1-signature-info-list)))
                     ;; SKip signature
                     (xmp-file-reader-skip reader (nth 1 sig-info))
                     ;; Read bytes
                     (push (cons
                            (nth 2 sig-info)
                            (xmp-file-reader-read-bytes reader
                                                        (- seglen
                                                           2
                                                           (nth 1 sig-info))))
                           result)
                     ;; Remove from list
                     (unless (nth 3 sig-info)
                       (setq app1-signature-info-list
                             (delq sig-info app1-signature-info-list)))))
                 (xmp-file-reader-seek reader (+ segoff seglen)))
               t))))
      (nreverse result))))
;; EXAMPLE: (xmp-file-scan-jpeg-app1 "test/xmp-test-uzumaki.jpg" (list xmp-file-scan-jpeg-exif-signature-info xmp-file-scan-jpeg-xmp-signature-info))

;; TODO: Support ExtendedXMP
;; ExtendedXMP
;; ((looking-at-p xmp-file-scan-jpeg-xmp-ext-signature)
;;  (xmp-file-reader-skip
;;   reader
;;   xmp-file-scan-jpeg-xmp-ext-signature-size)
;;  (let ((guid
;;         (xmp-file-reader-read-bytes reader 32))
;;        (full-size
;;         (xmp-file-reader-u32 reader))
;;        (portion-offset
;;         (xmp-file-reader-u32 reader))
;;        (portion-bytes
;;         (xmp-file-reader-read-bytes
;;          reader
;;          (- seglen
;;             2
;;             xmp-file-scan-jpeg-xmp-ext-signature-size
;;             32 4 4))))
;;    (push (list guid full-size portion-offset portion-bytes)
;;          extended-xmp-portions)))

;; TODO: Combine ExtendedXMP to StandardXMP
;;       - Retrieve GUID from xmpNote:HasExtendedXMP in standard-xmp
;;       - Reject invalid GUID
;;       - Sort portions by offset
;;       - Check continuity
;;       - Check total size
;;       - Combine StandardXMP and ExtendedXMP
;;       - Remove xmpNote:HasExtendedXMP
;; I don't have any JPEG files using ExtendedXMP on hand, so I
;; won't implement it now.

(autoload 'xmp-exif-read-exif-as-xmp-property-elements-from-bytes "xmp-exif")

(defun xmp-file-read-xml-from-jpeg (file)
  (let* ((segments (xmp-file-scan-jpeg-app1
                    file
                    (nconc
                     (when xmp-jpeg-extract-exif-p
                       (list xmp-file-scan-jpeg-exif-signature-info))
                     (list  xmp-file-scan-jpeg-xmp-signature-info))))
         (dom
          (when-let ((xmp-packet (alist-get 'standard-xmp segments)))
            (xmp-xml-parse-string
             (decode-coding-string xmp-packet 'utf-8))))
         (exif-prop-elem-list
          (when-let ((exif-bytes (alist-get 'exif segments)))
            (xmp-exif-read-exif-as-xmp-property-elements-from-bytes
             exif-bytes
             ;; TODO: Add partial read feature
             nil))))

    (xmp-merge-xml-dom-and-property-elements dom exif-prop-elem-list)))

(defun xmp-merge-xml-dom-and-property-elements (dom prop-elem-list)
  "Merge the contents of PROP-ELEM-LIST into the DOM.
However, if the same property exists in both, the property in DOM takes
precedence.

If DOM is nil or invalid, a new empty DOM is created and the
PROP-ELEM-LIST is inserted into it."
  (when dom
    ;; Remove all xmlns:??= attributes that exist on non-root elements.
    ;; When the `xmp-xml-element-attributes' returns namespace
    ;; declarations, there are a few places where it will not work
    ;; correctly.
    (xmp-xml-move-nsdecls-to-root dom))

  (when prop-elem-list
    ;; If DOM is invalid, discard it and recreate dom.
    (when (or (null dom)
              (not (xmp-find-rdf dom)))
      (setq dom (xmp-empty-dom)))

    ;; Insert PROP-ELEM-LIST into DOM
    (xmp-set-property-elements-if-not-exists dom prop-elem-list))

  dom)

;;;;; TIFF File

(autoload 'xmp-exif-read-xmp-xml-from-tiff-file "xmp-exif")

(defun xmp-file-read-xml-from-tiff (file)
  (let* ((dom-exif (xmp-exif-read-xmp-xml-from-tiff-file file))
         (dom (car dom-exif))
         (exif-prop-elem-list (cdr dom-exif)))

    (xmp-merge-xml-dom-and-property-elements dom exif-prop-elem-list)))

;;;;; PDF File

(defcustom xmp-file-pdfinfo-program
  (executable-find "pdfinfo")
  "The path to pdfinfo command or nil if none.

By default, this variable is set to the path where pdfinfo exists when
elisp is loaded. If you install pdfinfo later, you should change this
variable explicitly."
  :group 'xmp
  :type '(choice (const :tag "Do not use pdfinfo" nil)
                 file))

(autoload 'xmp-pdf-read-metadata "xmp-pdf")

(defun xmp-file-read-xml-from-pdf (file)
  (or
   ;; Use pdfinfo
   (and xmp-file-pdfinfo-program
        (ignore-errors
          (xmp-xml-move-nsdecls-to-root
           (xmp-xml-parse-string
            (with-temp-buffer
              (let* ((default-process-coding-system '(utf-8 . utf-8))
                     (status (call-process xmp-file-pdfinfo-program
                                           nil t nil "-meta"
                                           (expand-file-name file))))
                (unless (eq status 0)
                  (error "pdfinfo exited with status %s" status))
                (buffer-substring-no-properties (point-min) (point-max))))))))
   ;; Use elisp implementation (Encryption, compression, etc. are not supported)
   (when-let ((metadata (ignore-errors (xmp-pdf-read-metadata file)))
              (bytes (plist-get metadata :bytes)))
     (xmp-xml-move-nsdecls-to-root
      (xmp-xml-parse-string
       (decode-coding-string bytes 'utf-8))))
   ;; Search xpacket (There is a possibility of reading the wrong packet.)
   (xmp-file-read-xml-from-scanned-packet file)))


;;;;; XML File

(defun xmp-file-read-xml-from-xmp-xml (file)
  ;; Remove all xmlns:??= attributes that exist on non-root elements.
  ;; When the `xmp-xml-element-attributes' returns namespace
  ;; declarations, there are a few places where it will not work
  ;; correctly.
  (xmp-xml-move-nsdecls-to-root
   (xmp-xml-parse-file file)))

(defun xmp-file-write-xml-to-xmp-xml (file dom)
  (xmp-xml-write-file file dom xmp-xml-default-ns-name-prefix-alist))

;;;;; File Handlers

;; TODO: Implement more handlers for GIF, PNG, etc. (see [XMP3])
(defconst xmp-file-name-handler-alist
  '(("\\.[Xx][Mm][Ll]$"
     :read-xml xmp-file-read-xml-from-xmp-xml
     :write-xml xmp-file-write-xml-to-xmp-xml)
    ("\\.[Xx][Mm][Pp]$"
     :read-xml xmp-file-read-xml-from-xmp-xml
     :write-xml xmp-file-write-xml-to-xmp-xml)
    ("\\.[Jj][Pp][Ee]?[Gg]$"
     :read-xml xmp-file-read-xml-from-jpeg
     ;; TODO: Implement jpeg writer
     :write-xml xmp-file-write-xml-to-scanned-packet)
    ("\\.[Pp][Dd][Ff]$"
     :read-xml xmp-file-read-xml-from-pdf
     ;; TODO: Implement pdf writer
     :write-xml nil)
    ("\\.\\(?:[Tt][Ii][Ff][Ff]?\\|[Aa][Rr][Ww]\\|[Cc][Rr]2\\|[Dd][Nn][Gg]\\|[Nn][Ee][Ff]\\)\\'"
     :read-xml xmp-file-read-xml-from-tiff
     ;; TODO: Implement writer
     :write-xml nil)
    ))

(defconst xmp-file-magic-handler-alist
  '(("<?xml "
     :read-xml xmp-file-read-xml-from-xmp-xml
     :write-xml xmp-file-write-xml-to-xmp-xml)))

(defconst xmp-file-default-handler
  '(:read-xml
    xmp-file-read-xml-from-scanned-packet
    :write-xml
    xmp-file-write-xml-to-scanned-packet))

(defun xmp-file-find-handler (file operation)
  (plist-get
   (or
    ;; Test file name
    (or
     ;; case-sensitive
     (and (not (file-name-case-insensitive-p file))
          (let ((case-fold-search nil))
            (assoc-default file xmp-file-name-handler-alist #'string-match-p)))
     ;; case-insensitive
     (let ((case-fold-search t))
       (assoc-default file xmp-file-name-handler-alist #'string-match-p)))
    ;; Test file contents
    (when (file-regular-p file)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file nil 0 1024)
        (goto-char (point-min))
        (cl-loop for (regexp . handler) in xmp-file-magic-handler-alist
                 when (let ((case-fold-search nil))
                        (looking-at-p regexp))
                 return handler)))
    ;; Fallback
    xmp-file-default-handler)
   operation))

(defun xmp-file-op (operation file &rest args)
  (apply (or (xmp-file-find-handler file operation)
             (error "Failed to find file handler"))
         file
         args))

(defun xmp-file-read-xml (file)
  (xmp-file-op :read-xml file))

(defun xmp-file-write-xml (file dom)
  (xmp-file-op :write-xml file dom))

;;;;; Get/Set XMP Properties from/to File

(defun xmp-file-read-rdf (file)
  "Read the XML from a FILE and ensure that the RDF element is present.

XML is read using `xmp-file-read-xml'.

Return the root node of the XML read. The root node is either an
x:xmpmeta element or an rdf:RDF element.

If no RDF element is found in the XML, an error is signaled.
This is because the XML read may not be XMP."
  (let ((dom (xmp-file-read-xml file)))
    (unless (xmp-find-rdf dom)
      (error "No RDF elements in file: %s" file))
    dom))

(defun xmp-file-read-rdf-or-new-dom (file)
  "If FILE exists, call `xmp-file-read-xml' to read the FILE.
If it does not exist, call `xml-empty-dom' to return a DOM that does not
contain any properties yet.

Return the root node of the XML read. The root node is either an
x:xmpmeta element or an rdf:RDF element.

If no RDF element is found in the XMP XML, an error is signaled.
This is because the XML read may not be XMP."
  (let ((dom (if (file-exists-p file)
                 (xmp-file-read-xml file)
               (xmp-empty-dom))))
    (unless (xmp-find-rdf dom)
      (error "No RDF elements in file: %s" file))
    dom))

(defun xmp-file-remove-properties (file prop-ename-list-or-all
                                        &optional about use-cache)
  "Remove XMP properties from FILE."
  (let ((dom (xmp-file-read-rdf-or-new-dom file)))

    ;; Modify
    (if (eq prop-ename-list-or-all 'all)
        ;; Remove all properties
        (xmp-remove-all-descriptions dom about)
      ;; Remove specified properties
      (dolist (prop-ename prop-ename-list-or-all)
        (when-let ((desc (xmp-find-description dom prop-ename about)))
          (xmp-desc-remove-property desc prop-ename))))

    ;; Write (Do not delete FILE even if it has no XMP properties)
    (xmp-file-write-xml file dom)

    (when (xmp-file-cache-enabled use-cache)
      (xmp-file-cache-make-entry file dom))

    nil))

(defun xmp-file-set-properties (file prop-ename-value-alist
                                     &optional about use-cache)
  "Change XMP properties in FILE.

Read the XMP metadata from FILE, modify the properties specified in
PROP-ENAME-VALUE-ALIST, and write back to FILE.

PROP-ENAME-VALUE-ALIST is a list of properties, where each element is a
cons cell of an expanded name and a value.  The value must be in a
format recognized by the `xmp-property-element-from' function.

ABOUT specifies a string that matches the about attribute of Description
elements.

This function is used to write to a specified file. If you just want to
set metadata about what the file is, without specifying where to write
to, use `xmp-set-file-properties'."
  (let ((dom (xmp-file-read-rdf-or-new-dom file)))

    ;; Modify
    (cl-loop for (prop-ename . value) in prop-ename-value-alist
             do
             (let ((desc (or
                          (xmp-find-description dom prop-ename about)
                          (xmp-find-description dom nil about)
                          (xmp-xml-element-insert-last
                           dom
                           (xmp-empty-top-description about)))))
               (xmp-desc-set-property-value desc prop-ename value)))

    ;; Write
    (xmp-file-write-xml file dom)

    (when (xmp-file-cache-enabled use-cache)
      (xmp-file-cache-make-entry file dom))

    prop-ename-value-alist))

(defun xmp-file-set-property (file prop-ename value &optional about)
  "Change XMP property in FILE.

This function just calls `xmp-file-set-properties' on an alist with one
element."
  (xmp-file-set-properties file (list (cons prop-ename value)) about)
  value)
;; EXAMPLE: (xmp-file-set-property "test/tmp/xml-example-for-write.xmp" (xmp-xml-ename xmp-xmp: "Rating") "3")
;; EXAMPLE: (xmp-file-set-property "test/tmp/xmp-test-uzumaki.jpg" (xmp-xml-ename xmp-xmp: "Rating") "3")

(defun xmp-file-get-properties (file
                                prop-ename-list
                                &optional about noerror
                                dst-ns-name-prefix-alist
                                use-cache)
  "Return a list of XMP properties contained in the FILE.

PROP-ENAME-LIST is a list of the expanded names of the properties to
retrieve or the symbol `all'. nil means to retrieve none. The symbol
`all' means to enumerate all properties. The order or duplication of
properties in the list does not affect the value returned.

ABOUT specifies a string that matches the about attribute of Description
elements. Get properties only from matching Description
elements. Specifying nil is the same as specifying an empty string.

If NOERROR is non-nil, then nil is returned if an error occurs, whenever
possible.

Return a list of cons cells whose car is the expanded name and whose cdr
is the pvalue (parsed value: returned by `xmp-parse-property-element').

This function is used to read from a specified file. If you just want to
get metadata about what the file is, without specifying where to read
from, use `xmp-get-file-properties'."
  (when prop-ename-list
    (let ((cached-result (if (xmp-file-cache-enabled use-cache)
                             ;; 'no-cache | 'not-covered | <property-alist-or-nil>
                             (xmp-file-cache-get-properties
                              file prop-ename-list dst-ns-name-prefix-alist)
                           'do-not-use-cache)))
      (if (listp cached-result) ;; Not 'no-cache, 'not-covered
          ;; From cache
          cached-result
        ;; From FILE
        (when-let ((dom (if noerror
                            ;; non-nil or nil(error)
                            (ignore-errors (xmp-file-read-rdf file))
                          ;; non-nil or error
                          (xmp-file-read-rdf file))))

          ;; Make a cache entry
          ;; Do not update when `not-covered' or `do-not-use-cache'
          (when (eq cached-result 'no-cache)
            (xmp-file-cache-make-entry file dom))

          ;; Collect namespace prefixes
          (when (consp dst-ns-name-prefix-alist)
            (nconc dst-ns-name-prefix-alist
                   (xmp-xml-collect-nsdecls dom)))

          ;; Return properties
          (xmp-get-properties dom prop-ename-list about noerror))))))
;; TEST: (xmp-file-get-properties "test/xmp-test-syntax-property-elements.xmp" (list (xmp-xml-ename (xmp-xml-ns-name "http://misohena.jp/ns1/") "LiteralPropElt1") (xmp-xml-ename (xmp-xml-ns-name "http://misohena.jp/ns1/") "LiteralPropElt2"))) => (((:http://misohena.jp/ns1/ . "LiteralPropElt1") :pv-type text :value "LiteralPropElt1Val") ((:http://misohena.jp/ns1/ . "LiteralPropElt2") :pv-type text :value "LiteralPropElt1Val" :qualifiers (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text :value "ja"))))
;; TEST: (xmp-file-get-properties "test/xmp-test-syntax-property-elements.xmp" (list (xmp-xml-ename (xmp-xml-ns-name "http://misohena.jp/ns1/") "LiteralPropElt1") (xmp-xml-ename (xmp-xml-ns-name "http://misohena.jp/ns1/") "LiteralPropElt1"))) => (((:http://misohena.jp/ns1/ . "LiteralPropElt1") :pv-type text :value "LiteralPropElt1Val"))
;; TEST: (xmp-file-get-properties "test/xmp-test-syntax-property-elements.xmp" (list (xmp-xml-ename (xmp-xml-ns-name "http://misohena.jp/ns1/") "ResPropElt3"))) => (((:http://misohena.jp/ns1/ . "ResPropElt3") :pv-type array :value ((:pv-type text :value "ResPropElt3Item1") (:pv-type text :value "ResPropElt3Item2")) :qualifiers (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text :value "ja")) :array-type (:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "Bag")))

(defun xmp-file-enumerate-properties (file
                                      &optional prop-ename-list about noerror
                                      dst-ns-name-prefix-alist
                                      use-cache)
  "Return a list of XMP properties contained in the FILE.

Same as `xmp-file-get-properties', but PROP-ENAME-LIST is optional and
nil means to get all properties."
  (xmp-file-get-properties file
                           (or prop-ename-list 'all)
                           about noerror
                           dst-ns-name-prefix-alist use-cache))

(defun xmp-file-get-property (file prop-ename &optional about noerror)
  "Return the XMP property contained in the FILE.

This function calls `xmp-file-get-properties' with a single-element list."
  (cdr (car (xmp-file-get-properties file (list prop-ename)
                                     about noerror))))
;; TEST: (xmp-pvalue-as-text (xmp-file-get-property "test/xmp-test-uzumaki.jpg" (xmp-xml-ename xmp-xmp: "Rating"))) => "5"
;; TEST: (xmp-file-get-property "test/xmp-test-uzumaki.jpg" (xmp-xml-ename xmp-dc: "creator")) => (:pv-type array :value ((:pv-type text :value "AKIYAMA Kouhei")) :array-type (:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "Seq"))


;;;; File Cache

(autoload 'xmp-sqlite-cache-db-make-file-entry "xmp-sqlite")
(autoload 'xmp-sqlite-cache-db-get-file-entry "xmp-sqlite")
(autoload 'xmp-sqlite-cache-db-remove-file-entry "xmp-sqlite")
(autoload 'xmp-sqlite-cache-db-remove-file-entries-in-dir "xmp-sqlite")
(autoload 'xmp-sqlite-cache-db-remove-invalid-file-entries "xmp-sqlite")
(autoload 'xmp-sqlite-cache-db-remove-invalid-file-entries-in-dir "xmp-sqlite")
(autoload 'xmp-sqlite-cache-db-remove-dir-entries "xmp-sqlite")
(autoload 'xmp-sqlite-cache-db-clear "xmp-sqlite")
(autoload 'xmp-sqlite-cache-db-get-files-in-dir "xmp-sqlite")

;;;;; Cache Target Properties

(defvar xmp-file-cache-target-prop-ename-list nil
  "A list of expanded names of properties that are cache targets.
Generated from the variable `xmp-file-cache-target-properties' by the
function `xmp-file-cache-target-properties-update'.

Use the function `xmp-file-cache-target-prop-ename-list' instead of
referencing this variable directly.")

(defvar xmp-file-cache-target-properties) ;; Forward Declaration

(defun xmp-file-cache-target-properties-update ()
  "Perform update processing when the customization variable
`xmp-file-cache-target-properties' is changed.

Update the variable `xmp-file-cache-target-prop-ename-list'."
  (setq xmp-file-cache-target-prop-ename-list
        (cl-loop for (prop-name _ns-prefix) in xmp-file-cache-target-properties
                 for ename = (xmp-xml-ename-ensure prop-name)
                 unless (xmp-xml-ename-member ename result)
                 collect ename into result
                 finally return (sort result #'xmp-xml-ename<))))

(defcustom xmp-file-cache-target-properties
  '((("http://ns.adobe.com/xap/1.0/" . "Rating") "xmp")
    (("http://ns.adobe.com/xap/1.0/" . "Label") "xmp")
    (("http://ns.adobe.com/xap/1.0/" . "CreateDate") "xmp")
    (("http://ns.adobe.com/xap/1.0/" . "ModifyDate") "xmp")
    (("http://purl.org/dc/elements/1.1/" . "title") "dc")
    (("http://purl.org/dc/elements/1.1/" . "description") "dc")
    (("http://purl.org/dc/elements/1.1/" . "subject") "dc")
    (("http://purl.org/dc/elements/1.1/" . "creator") "dc")
    (("http://purl.org/dc/elements/1.1/" . "date") "dc")
    (("http://ns.adobe.com/exif/1.0/" . "DateTimeOriginal") "exif")
    (("http://ns.adobe.com/exif/1.0/" . "DateTimeDigitized") "exif")
    )
  "A list that holds information about the properties that are cached.

To get a list of cache target properties, use the function
`xmp-file-cache-target-prop-ename-list'."
  :group 'xmp
  ;; TODO: Allow nil to be specified to cache all properties.
  :type '(repeat
          (list
           (cons
            :tag "Property name"
            (string :tag "Namespace name (URI)")
            (string :tag "Property local name"))
           (string :tag "Namespace prefix")))
  :set (lambda (var val)
         (set var val)
         ;; Update
         (xmp-file-cache-target-properties-update)))

(defun xmp-file-cache-target-prop-ename-list ()
  "Return a list of expanded names of properties to be cached."
  (when (and (null xmp-file-cache-target-prop-ename-list)
             xmp-file-cache-target-properties)
    (xmp-file-cache-target-properties-update))
  xmp-file-cache-target-prop-ename-list)

(defun xmp-file-cache-target-properties-p (prop-ename-list)
  "Return non-nil if PROP-ENAME-LIST contains only the names of cache
target properties specified `xmp-file-cache-target-properties'."
  (and (listp prop-ename-list) ;; Not to get all properties (not (eq 'all))
       prop-ename-list
       (seq-every-p
        (lambda (prop-ename)
          (xmp-xml-ename-member prop-ename
                                (xmp-file-cache-target-prop-ename-list)))
        prop-ename-list)))
;; TEST: (xmp-file-cache-target-properties-p nil) => nil
;; TEST: (xmp-file-cache-target-properties-p 'all) => nil
;; EXAMPLE: (xmp-file-cache-target-properties-p (list xmp-xmp:Rating xmp-dc:title))

(defun xmp-file-cache-nconc-ns-name-prefix-alist (dst-ns-name-prefix-alist)
  "Create a namespace names and prefixes alist from
`xmp-file-cache-target-properties' and concatenate it to the end of
DST-NS-NAME-PREFIX-ALIST."
  (when dst-ns-name-prefix-alist
    (nconc
     dst-ns-name-prefix-alist
     ;; Collect ns names and prefixes from `xmp-file-cache-target-properties'.
     (cl-loop with ns-names = nil
              for (prop-name ns-prefix) in xmp-file-cache-target-properties
              for prop-ename = (xmp-xml-ename-ensure prop-name)
              for ns-name = (xmp-xml-ename-ns prop-ename)
              unless (assq ns-name ns-names)
              do (push (cons ns-name ns-prefix) ns-names)
              finally return ns-names))))

;;;;; Cache Enable

(defcustom xmp-file-cache-enabled t
  "Non-nil means enable file metadata caching."
  :type 'boolean
  :group 'xmp)

(defun xmp-file-cache-enabled (&optional use-cache)
  "Return non-nil if file caching is enabled."
  (pcase use-cache
    ('t t)
    ('no-cache nil)
    (_ xmp-file-cache-enabled)))

(defvar xmp-sqlite-available-p
  (and (fboundp 'sqlite-available-p)
       (sqlite-available-p))
  "A variable that holds whether the current Emacs session can use SQLite.")

(defcustom xmp-file-cache-sqlite-enabled t
  "Non-nil means use SQLite (if available) for file metadata caching."
  :group 'xmp
  :type 'boolean)

(defun xmp-file-cache-sqlite-enabled-p ()
  (and xmp-sqlite-available-p
       xmp-file-cache-sqlite-enabled))

;;;;; Cache Maintenance

(defun xmp-clear-file-cache ()
  "Clear all file metadata caches."
  (interactive
   (unless (yes-or-no-p
            (xmp-msg "Do you want to clear all file metadata caches?"))
     (error "Abort"))
   nil)
  (xmp-file-cache-memory-clear)
  (xmp-sqlite-cache-db-clear))

(defun xmp-clear-file-cache-in-dir (dir)
  "Clear file metadata caches in DIR."
  (interactive "D")
  (setq dir (expand-file-name dir))
  (message
   (xmp-msg "Number of cache entries deleted:\nIn-memory: %d\nIn DB: %d")
   (xmp-file-cache-memory-remove-dir-entry dir)
   (xmp-sqlite-cache-db-remove-file-entries-in-dir dir)))

(defun xmp-clear-file-cache-under-dir (dir)
  "Clear file metadata caches under DIR."
  (interactive "D")
  (setq dir (expand-file-name dir))
  (message
   (xmp-msg "Number of cache entries deleted:\nIn-memory: %d\nIn DB: %d")
   (xmp-file-cache-memory-remove-dir-entries-if
    (lambda (entry-dir) (string-prefix-p dir entry-dir)))
   (xmp-sqlite-cache-db-remove-dir-entries
    (lambda (entry-dir) (string-prefix-p dir entry-dir)))))

(defun xmp-clear-invalid-file-cache ()
  "Clear invalid file metadata caches."
  (interactive)
  (message
   (xmp-msg "Number of cache entries deleted:\nIn-memory: %d\nIn DB: %d")
   (xmp-file-cache-memory-remove-invalid-file-entries)
   (xmp-sqlite-cache-db-remove-invalid-file-entries)))

(defun xmp-clear-invalid-file-cache-in-dir (dir)
  "Clear invalid file metadata caches in DIR."
  (interactive "D")
  (setq dir (expand-file-name dir))
  (message
   (xmp-msg "Number of cache entries deleted:\nIn-memory: %d\nIn DB: %d")
   (xmp-file-cache-memory-remove-invalid-file-entries-in-dir dir)
   (xmp-sqlite-cache-db-remove-invalid-file-entries-in-dir dir)))

(defun xmp-clear-invalid-file-cache-under-dir (dir)
  "Clear invalid file metadata caches under DIR."
  (interactive "D")
  (setq dir (expand-file-name dir))
  (message
   (xmp-msg "Number of cache entries deleted:\nIn-memory: %d\nIn DB: %d")
   (xmp-file-cache-memory-remove-invalid-file-entries
    (lambda (entry-dir) (string-prefix-p dir entry-dir)))
   (xmp-sqlite-cache-db-remove-invalid-file-entries
    (lambda (entry-dir) (string-prefix-p dir entry-dir)))))

;;;;; Get/Set Cache

(defun xmp-file-cache-get-properties (file
                                      prop-ename-list
                                      &optional dst-ns-name-prefix-alist)
  "Return the cached properties of the FILE.

PROP-ENAME-LIST is a list of property names to get. If this list
contains a property that is not a cache target, return `not-covered'.

If there is no valid cache, return `no-cache'.

If all properties specified in PROP-ENAME-LIST are cache targets and a
valid cache exists, return only the properties specified in
PROP-ENAME-LIST from the cache. Properties that do not exist in the file
are not included in the result. If none of the properties specified in
PROP-ENAME-LIST exist in the file, return nil."
  ;; All properties specified in PROP-ENAME-LIST must be cache targets.
  (if (xmp-file-cache-target-properties-p prop-ename-list)
      (let ((file-attrs (file-attributes file))
            (result 'undecided)
            (sources '(mem db)))

        ;; Try to get the cache entry from in memory, then from the database.
        (while (and (eq result 'undecided) sources)
          (let* ((from-mem (eq (pop sources) 'mem))
                 (file-entry (if from-mem
                                 (xmp-file-cache-memory-get-file-entry file)
                               (when (xmp-file-cache-sqlite-enabled-p)
                                 (xmp-sqlite-cache-db-get-file-entry file)))))
            (cond
             ;; No cache => Do nothing and next source
             ((null file-entry))
             ;; Invalid cache => discard cache and return `no-cache'
             ((not (xmp-file-cache-file-entry-valid-p file-entry
                                                      file-attrs
                                                      prop-ename-list))
              (when from-mem
                (xmp-file-cache-memory-remove-file-entry file))
              (when (xmp-file-cache-sqlite-enabled-p)
                (xmp-sqlite-cache-db-remove-file-entry file))
              (setq result 'no-cache))
             ;; Valid cache (Use FILE-ENTRY)
             (t
              ;; Reflect upper-level cache
              (unless from-mem
                (xmp-file-cache-memory-make-file-entry-from-db
                 file file-entry))
              ;; Output namespace prefix alist
              (xmp-file-cache-nconc-ns-name-prefix-alist
               dst-ns-name-prefix-alist)
              ;; Return properties
              (setq result
                    (xmp-file-cache-file-entry-get-properties
                     file-entry prop-ename-list))))))

        (when (eq result 'undecided)
          (setq result 'no-cache))
        result)
    'not-covered))

(defun xmp-file-cache-make-entry (file dom)
  "Create a metadata cache entry for FILE from the DOM.
If an error or other irregularity occurs, return without doing anything."
  (let ((file-attrs (file-attributes file)))
    (when (and file-attrs  ;; file exists
               (not (eq (file-attribute-type file-attrs) t))) ;; not directory
      (let* ((target-prop-ename-list (xmp-file-cache-target-prop-ename-list))
             (properties
              (condition-case nil
                  ;; If parse error, signal error and don't create the entry.
                  ;; Note: PROPERTIES can be nil.
                  (xmp-file-cache-collect-target-property-values
                   dom
                   target-prop-ename-list)
                (error 'error))))
        (unless (eq properties 'error)
          ;; In-memory cache
          (xmp-file-cache-memory-make-file-entry file file-attrs properties
                                                 target-prop-ename-list)
          ;; Database cache
          (when (xmp-file-cache-sqlite-enabled-p)
            (xmp-sqlite-cache-db-make-file-entry file file-attrs properties
                                              target-prop-ename-list)))))))

(defun xmp-file-cache-remove-file-entry (file)
  (xmp-file-cache-memory-remove-file-entry file)
  (xmp-sqlite-cache-db-remove-file-entry file))

(defun xmp-file-cache-collect-target-property-values (dom
                                                      target-prop-ename-list)
  "Create an alist of property names and values to register in file
metadata cache entries.

Get property elements from the DOM. (Strictly speaking, it may be
obtained from attributes, not necessarily elements.)

Get only the properties specified in TARGET-PROP-ENAME-LIST If the
property is retrieved from the DOM, the resulting alist contains (ENAME
. PVALUE). If the property does not exist in the DOM, the resulting
alist does not contain a cell for that property.

Signal an error if an error occurs while parsing the element."
  (cl-loop
   for prop-ename in target-prop-ename-list
   for prop-elem = (xmp-get-property-element dom prop-ename)
   when prop-elem
   ;; Collect (PROP-ENAME . PVALUE )
   ;; Note: A nil PVALUE means an empty value
   ;; (<Prop></Prop> => (Prop . nil not ""))
   collect (xmp-parse-property-element prop-elem
                                       ;; If parse error, signal error.
                                       nil)))


;;;;; In-memory cache

;; - File Entry
;; - Directory Entry
;; - Directory Entry Table (`xmp-file-cache-dirs')

;;;;;; File Entry

(defun xmp-file-cache-file-entry-make (filename modtime properties
                                                target-prop-ename-list)
  "Construct an xmp-file-cache-entry object."
  (list filename modtime properties target-prop-ename-list))

(defmacro xmp-file-cache-file-entry-file-name (file-entry)
  "Return FILE-ENTRY's file name without directory part."
  `(car ,file-entry))

(defmacro xmp-file-cache-file-entry-modtime (file-entry)
  "Return FILE-ENTRY's modification time.

This is typically a list representing a time, but could also be a float
type (e.g. when restoring from a database)."
  `(cadr ,file-entry))

(defmacro xmp-file-cache-file-entry-properties (file-entry)
  "Return FILE-ENTRY's properties."
  `(caddr ,file-entry))

(defmacro xmp-file-cache-file-entry-target-prop-ename-list (file-entry)
  "Return a list of the expanded names of the properties that the
FILE-ENTRY targets for caching."
  `(cadddr ,file-entry))

(defsubst xmp-file-cache-file-entry-full-path (file-entry dir)
  (file-name-concat dir (xmp-file-cache-file-entry-file-name file-entry)))

(defconst xmp-file-cache-time-tolerance 1e-6) ;; 1 microsecond

(defun xmp-file-cache-time-equal-p (time1 time2)
  ;; Since (xmp-file-cache-file-entry-modtime file-entry) may be a
  ;; float (when using DB cache), always calculate as floats.
  (< (abs (- (float-time time1)
             (float-time time2)))
     xmp-file-cache-time-tolerance))

(defun xmp-file-cache-file-entry-modtime-equal-p (file-entry modtime)
  ;; Since (xmp-file-cache-file-entry-modtime file-entry) may be a
  ;; float (when using DB cache), always calculate as floats.
  (xmp-file-cache-time-equal-p
   (xmp-file-cache-file-entry-modtime file-entry)
   modtime))

(defun xmp-file-cache-file-entry-valid-p (file-entry file-attrs prop-ename-list)
  "Return non-nil if FILE-ENTRY is valid."
  (and file-entry ;; The file is cached
       file-attrs ;; The file exists
       ;; The file has not been modified
       (xmp-file-cache-file-entry-modtime-equal-p
        file-entry
        (file-attribute-modification-time file-attrs))
       ;; The cache target properties have not increased since the
       ;; file was cached.
       (xmp-file-cache-file-entry-targets-all-props-p file-entry
                                                      prop-ename-list)))

(defun xmp-file-cache-file-entry-invalid-p (file-entry dir)
  (not
   (xmp-file-cache-file-entry-valid-p
    file-entry
    (file-attributes (xmp-file-cache-file-entry-full-path file-entry dir))
    nil)))

(defun xmp-file-cache-file-entry-targets-all-props-p (file-entry
                                                      prop-ename-list)
  "Return non-nil if FILE-ENTRY targets all properties in PROP-ENAME-LIST
for caching."
  (seq-every-p
   (lambda (prop-ename)
     (xmp-xml-ename-member
      prop-ename
      (xmp-file-cache-file-entry-target-prop-ename-list file-entry)))
   prop-ename-list))

(defun xmp-file-cache-file-entry-get-properties (file-entry prop-ename-list)
  "Extract and return the properties specified by PROP-ENAME-LIST from
FILE-ENTRY."
  (when file-entry
    (cl-loop for ename in prop-ename-list
             for ename-pvalue = (xmp-xml-ename-assoc
                                 ename
                                 (xmp-file-cache-file-entry-properties
                                  file-entry))
             when ename-pvalue
             ;; TODO: No copying needed if DB is used.
             collect (copy-tree ename-pvalue))))

;;;;;; Dir Entry

;; ;; Alist Version
;; (defun xmp-file-cache-dir-entry-make (dir)
;;   (cons dir nil))
;; (defmacro xmp-file-cache-dir-entry-files (dir-entry)
;;   `(cdr ,dir-entry))
;; (defun xmp-file-cache-dir-entry-set-file-entry (dir-entry file-entry)
;;   (xmp-file-cache-dir-entry-remove-file-entry
;;    dir-entry
;;    (xmp-file-cache-file-entry-file-name file-entry))
;;   (push file-entry (xmp-file-cache-dir-entry-files dir-entry)))
;; (defun xmp-file-cache-dir-entry-get-file-entry (dir-entry file)
;;   (assoc (file-name-nondirectory file)
;;          (xmp-file-cache-dir-entry-files dir-entry)
;;          #'string=))
;; (defun xmp-file-cache-dir-entry-remove-file-entry (dir-entry file)
;;   (let ((file-entry (xmp-file-cache-dir-entry-get-file-entry dir-entry file)))
;;     (when file-entry
;;       (setf (xmp-file-cache-dir-entry-files dir-entry)
;;             (delq file-entry (xmp-file-cache-dir-entry-files dir-entry))))))
;; (defun xmp-file-cache-dir-entry-empty-p (dir-entry)
;;   (null (xmp-file-cache-dir-entry-files dir-entry)))

;; Hash Table Version
(defun xmp-file-cache-dir-entry-make (dir)
  (cons dir (make-hash-table :test 'equal)))
(defmacro xmp-file-cache-dir-entry-directory (dir-entry)
  `(car ,dir-entry))
(defmacro xmp-file-cache-dir-entry-files-hash (dir-entry)
  `(cdr ,dir-entry))
(defun xmp-file-cache-dir-entry-set-file-entry (dir-entry file-entry)
  (puthash (xmp-file-cache-file-entry-file-name file-entry)
           file-entry
           (xmp-file-cache-dir-entry-files-hash dir-entry)))
(defun xmp-file-cache-dir-entry-get-file-entry (dir-entry file)
  (gethash (file-name-nondirectory file)
           (xmp-file-cache-dir-entry-files-hash dir-entry)))
(defun xmp-file-cache-dir-entry-remove-file-entry (dir-entry file)
  (remhash (file-name-nondirectory file)
           (xmp-file-cache-dir-entry-files-hash dir-entry)))
(defun xmp-file-cache-dir-entry-empty-p (dir-entry)
  (= (hash-table-count (xmp-file-cache-dir-entry-files-hash dir-entry)) 0))
(defun xmp-file-cache-dir-entry-file-count (dir-entry)
  (hash-table-count (xmp-file-cache-dir-entry-files-hash dir-entry)))

(defun xmp-file-cache-dir-entry-remove-file-entries-if (dir-entry pred)
  (let ((dir (xmp-file-cache-dir-entry-directory dir-entry))
        (hashtable (xmp-file-cache-dir-entry-files-hash dir-entry))
        (num-removed 0))
    (maphash (lambda (file-name file-entry)
               (when (funcall pred file-entry dir)
                 (remhash file-name hashtable)
                 ;; (message "Remove In-memory cache  %s" file-name)
                 (cl-incf num-removed)))
             hashtable)
    num-removed))

(defun xmp-file-cache-dir-entry-remove-invalid-file-entries (dir-entry)
  (xmp-file-cache-dir-entry-remove-file-entries-if
   dir-entry
   #'xmp-file-cache-file-entry-invalid-p))

(defun xmp-file-cache-dir-entry-get-files (dir-entry)
  (let ((dir (xmp-file-cache-dir-entry-directory dir-entry))
        result)
    (maphash (lambda (file-name _file-entry)
               (push (file-name-concat dir file-name) result))
             (xmp-file-cache-dir-entry-files-hash dir-entry))
    (nreverse result)))

;;;;;; Directory Table (In-memory cache)

(defvar xmp-file-cache-memory-dirs nil)

;; Get

(defun xmp-file-cache-memory-get-dir-entry (dir)
  (setq dir (expand-file-name dir))
  (assoc dir xmp-file-cache-memory-dirs #'string=))

(defun xmp-file-cache-memory-get-dir-entry-create (dir)
  (setq dir (expand-file-name dir))
  (or (assoc dir xmp-file-cache-memory-dirs #'string=)
      (car (push (xmp-file-cache-dir-entry-make dir)
                 xmp-file-cache-memory-dirs))))

(defun xmp-file-cache-memory-get-file-entry (file)
  (when-let ((dir-entry (xmp-file-cache-memory-get-dir-entry
                         (file-name-directory (expand-file-name file)))))
    (xmp-file-cache-dir-entry-get-file-entry dir-entry file)))

(defun xmp-file-cache-memory-set-file-entry (file file-entry)
  (xmp-file-cache-dir-entry-set-file-entry
   (xmp-file-cache-memory-get-dir-entry-create
    (file-name-directory (expand-file-name file)))
   file-entry))

;; Set

(defun xmp-file-cache-memory-make-file-entry (file file-attrs properties
                                                   target-prop-ename-list)
  (xmp-file-cache-memory-set-file-entry
   file
   (xmp-file-cache-file-entry-make
    (file-name-nondirectory file)
    (file-attribute-modification-time file-attrs)
    properties
    target-prop-ename-list)))

(defun xmp-file-cache-memory-make-file-entry-from-db (file db-file-entry)
  (xmp-file-cache-memory-set-file-entry file db-file-entry))

;; Remove

(defun xmp-file-cache-memory-clear ()
  "Clear the in-memory cache."
  (interactive)
  (setq xmp-file-cache-memory-dirs nil))

(defun xmp-file-cache-memory-remove-dir-entry (dir)
  (setq dir (expand-file-name dir))
  (if-let ((dir-entry (xmp-file-cache-memory-get-dir-entry dir)))
      (progn
        (setq xmp-file-cache-memory-dirs
              (delq dir-entry xmp-file-cache-memory-dirs))
        ;; Return the number of removed files
        (xmp-file-cache-dir-entry-file-count dir-entry))
    0))

(defun xmp-file-cache-memory-remove-dir-entry-if-empty (dir-entry)
  (when (xmp-file-cache-dir-entry-empty-p dir-entry)
    (setq xmp-file-cache-memory-dirs
          (delq dir-entry xmp-file-cache-memory-dirs))))

(defun xmp-file-cache-memory-remove-dir-entries-if (pred)
  (let ((num-removed 0)
        (prev-cell nil)
        (curr-cell xmp-file-cache-memory-dirs))
    (while curr-cell
      (let* ((dir-entry (car curr-cell))
             (dir-path (xmp-file-cache-dir-entry-directory dir-entry)))
        (when (funcall pred dir-path)
          ;; Count
          (cl-incf num-removed (xmp-file-cache-dir-entry-file-count dir-entry))
          ;; Remove dir-entry
          (if prev-cell
              (setcdr prev-cell (cdr curr-cell))
            (setq xmp-file-cache-memory-dirs (cdr curr-cell)))))
      ;; Next dir entry
      (setq prev-cell curr-cell
            curr-cell (cdr curr-cell)))
    num-removed))

(defun xmp-file-cache-memory-remove-file-entry (file)
  (let* ((dir (file-name-directory (expand-file-name file)))
         (dir-entry (assoc dir xmp-file-cache-memory-dirs #'string=)))
    (when dir-entry
      (xmp-file-cache-dir-entry-remove-file-entry dir-entry file)
      (xmp-file-cache-memory-remove-dir-entry-if-empty dir-entry))))

(defun xmp-file-cache-memory-remove-file-entries-if (dir-pred file-pred)
  (let ((num-removed 0)
        (prev-cell nil)
        (curr-cell xmp-file-cache-memory-dirs))
    (while curr-cell
      (let ((dir-entry (car curr-cell)))
        (when (or (null dir-pred)
                  (funcall dir-pred
                           (xmp-file-cache-dir-entry-directory dir-entry)))
          ;; Remove invalid file entries
          (cl-incf
           num-removed
           (xmp-file-cache-dir-entry-remove-file-entries-if dir-entry
                                                            file-pred))
          ;; Remove dir-entry if empty
          (when (xmp-file-cache-dir-entry-empty-p dir-entry)
            (if prev-cell
                (setcdr prev-cell (cdr curr-cell))
              (setq xmp-file-cache-memory-dirs (cdr curr-cell))))))
      ;; Next dir entry
      (setq prev-cell curr-cell
            curr-cell (cdr curr-cell)))
    num-removed))

(defun xmp-file-cache-memory-remove-invalid-file-entries (&optional dir-pred)
  (xmp-file-cache-memory-remove-file-entries-if
   dir-pred
   #'xmp-file-cache-file-entry-invalid-p))

(defun xmp-file-cache-memory-remove-invalid-file-entries-in-dir (dir)
  (let ((num-removed 0))
    (when-let ((dir-entry (xmp-file-cache-memory-get-dir-entry dir)))
      (cl-incf
       num-removed
       (xmp-file-cache-dir-entry-remove-invalid-file-entries dir-entry))
      (xmp-file-cache-memory-remove-dir-entry-if-empty dir-entry))
    num-removed))

;; List

(defun xmp-file-cache-memory-get-files-in-dir (dir)
  (when-let ((dir-entry (xmp-file-cache-memory-get-dir-entry dir)))
    (xmp-file-cache-dir-entry-get-files dir-entry)))


;;;; Sidecar Files

(defun xmp-sidecar-file-p (file)
  (equal (file-name-extension file) "xmp"))

(defun xmp-sidecar-file-name-and-exists-p (target-file)
  (unless (xmp-sidecar-file-p target-file)
    ;; TODO: Customize sidecar file name rules
    (let ((sidecar-file-candidates
           (list (concat target-file ".xmp")
                 (concat (file-name-sans-extension target-file) ".xmp"))))
      (or
       ;; From existing files
       (when-let ((file (seq-find #'file-regular-p sidecar-file-candidates)))
         (cons file t))
       ;; From the first candidate
       (cons (car sidecar-file-candidates) nil)))))

(defun xmp-sidecar-file-name (target-file)
  "Return the file name of the sidecar file used to record the metadata of
TARGET-FILE.

Return nil if TARGET-FILE is in a format that cannot have a sidecar
file. For example, TARGET-FILE itself is a sidecar file (with the
extension .xmp).

If there is an existing file that can be recognized as a sidecar file,
return the file name. If not, return the default sidecar file name."
  (car (xmp-sidecar-file-name-and-exists-p target-file)))

(defun xmp-sidecar-file-target (sidecar-file)
  ;; TODO: Customize sidecar file name rules
  ;; TODO: Search foo.xmp => foo.jpg or etc.
  (file-name-sans-extension sidecar-file))

(defun xmp-sidecar-files-in-dir (dir)
  (when (file-directory-p dir)
    (seq-filter (lambda (file)
                  (and (xmp-sidecar-file-p file)
                       ;; Exclude directory with .xmp
                       (file-regular-p file)))
                (directory-files dir t))))

(defun xmp-sidecar-file-targets-in-dir (dir)
  (mapcar #'xmp-sidecar-file-target
          (xmp-sidecar-files-in-dir dir)))

;;;; XMP Properties of Files

;; Properties describing a file do not need to be embedded within the
;; file itself. Modifications to the properties can be stored
;; externally without directly altering the file.
;;
;; External file properties are XMP properties that are stored outside
;; the description target file. There are two locations where external
;; file properties can be stored:
;;
;; - Sidecar files
;; - DB file entries
;;
;; Both add new metadata (XMP properties) to target files without
;; modifying them.
;;
;; File metadata is set using `xmp-set-file-properties' and retrieved
;; using `xmp-get-file-properties'. These functions do not read or
;; write metadata directly to the specified files. Instead, they read
;; and write metadata to pre-configured storage locations (as
;; specified by `xmp-file-property-storage-type').

;;;;; Storage Location

(defcustom xmp-file-property-storage-type
  'sidecar
  "The type of storage location for modified file metadata.

The storage type can be one of the symbols `db', `sidecar-or-db', or
`sidecar'. Their meanings are as follows:

  - db : Save to the database. If a sidecar file exists, it will be ignored.

  - sidecar : Save to the sidecar file.

  - sidecar-or-db : Save to the sidecar file if there is one. If not,
    save to the database. If a sidecar file is found later, the contents
    of the database will be merged into the sidecar file and saved to
    the sidecar file thereafter.

If this variable value is a list, it specifies the storage type for each
file condition. The elements of the list are in the format (MATCHER
. STORAGE). Files that match MATCHER will have the storage type STORAGE.

MATCHER can be one of the following:
- (dir-under . DIRECTORY-STRING) :
  The prefix of the file's full pathname matches DIRECTORY-STRING.
- (dir-equal . DIRECTORY-STRING) :
  The directory in which the file is located matches DIRECTORY-STRING.
- (regexp . REGEXP-STRING) :
   The file's full pathname matches the regular expression REGEXP-STRING.
- (extensions . EXTENSION-STRING-LIST) :
  The file's extension (without the dot) matches one of the elements of
  EXTENSION-STRING-LIST.
- all :
  Matches all files unconditionally.

STORAGE is one of the symbols `db', `sidecar-or-db', or `sidecar'."
  :group 'xmp
  :type '(choice
          (const :tag "Sidecar file or DB" sidecar-or-db)
          (const :tag "Sidecar file only" sidecar)
          (const :tag "DB only" db)
          (repeat
           :tag "Decide for each file"
           (cons
            :tag "File and storage"
            (choice
             :tag "Matcher"
             (cons :tag "All Subdirectories"
                   (const :format "" dir-under)
                   directory)
             (cons :tag "Same Directory"
                   (const :format "" dir-equal)
                   directory)
             (cons :tag "Regexp"
                   (const :format "" regexp)
                   regexp)
             (cons :tag "Extensions"
                   (const :format "" extensions)
                   (repeat :tag "Extension (without dot)" (string :tag "")))
             (const :tag "All Files (unconditionally)" all))
            ;; Storage
            (choice
             :tag "Storage"
             (const :tag "Sidecar file or DB" sidecar-or-db)
             (const :tag "Sidecar file only" sidecar)
             (const :tag "DB only" db))))))

(defun xmp-file-property-storage-match-p (matcher file)
  "Return non-nil if MATCHER matches FILE.

Used in interpreting the `xmp-file-property-storage-type' variable."
  (setq file (expand-file-name file))
  (pcase matcher
    (`(dir-under . ,dir)
     (string-prefix-p dir file))
    (`(dir-equal . ,dir)
     (equal (file-name-directory file) dir))
    (`(regexp . ,regexp)
     (string-match-p regexp file))
    (`(extensions . ,extensions)
     (member (file-name-extension file) extensions))
    ('all t)))

(defun xmp-file-property-storage-type (file)
  "Return the type of where to store FILE's metadata (properties)."
  (pcase xmp-file-property-storage-type
    ('db 'db)
    ('sidecar-or-db 'sidecar-or-db)
    ('sidecar 'sidecar)
    ((pred consp)
     (cl-loop for (matcher . storage) in xmp-file-property-storage-type
              when (xmp-file-property-storage-match-p matcher file)
              return storage))
    (_ ;; Including nil
     'sidecar-or-db)))

(defun xmp-file-property-storage-location (file)
  "Return the storage location of FILE metadata (properties).

If the storage location is a database, return the symbol db. If it is a
sidecar file, return the file name as a string."
  (let* ((storage-type (xmp-file-property-storage-type file))
         (sidecar-file-and-exists-p
          (when (or (eq storage-type 'sidecar)
                    (eq storage-type 'sidecar-or-db))
            (or
             (xmp-sidecar-file-name-and-exists-p file)
             ;; target-file is .xmp
             (cons file 'unknown)))))
    ;; Resolve sidecar or DB
    (when (eq storage-type 'sidecar-or-db)
      (setq storage-type
            (if (and xmp-sqlite-available-p
                     (not
                      (if (eq (cdr sidecar-file-and-exists-p) 'unknown)
                          (file-regular-p (car sidecar-file-and-exists-p))
                        (cdr sidecar-file-and-exists-p))))
                'db
              'sidecar)))
    (if (eq storage-type 'db)
        'db
      (car sidecar-file-and-exists-p))))
;; EXAMPLE: (xmp-file-property-storage-location "C:/home/a.jpg")

(autoload 'xmp-sqlite-mod-db-set-file-properties "xmp-sqlite")
(autoload 'xmp-sqlite-mod-db-get-file-properties-info "xmp-sqlite")
(autoload 'xmp-sqlite-mod-db-get-file-properties "xmp-sqlite")
(autoload 'xmp-sqlite-mod-db-remove-file-properties "xmp-sqlite")
(autoload 'xmp-sqlite-mod-db-remove-file-properties-all "xmp-sqlite")
(autoload 'xmp-sqlite-mod-db-get-files-in-dir "xmp-sqlite")

(defun xmp-file-merge-db-entry-into-sidecar-file (target-file sidecar-file)
  (when xmp-sqlite-available-p
    (when-let* ((db-properties-info
                 (xmp-sqlite-mod-db-get-file-properties-info target-file))
                (db-properties (plist-get db-properties-info :properties))
                ;; (db-modtime (plist-get db-properties-info :modtime))
                )
      ;; (let ((sidecar-properties (xmp-file-get-properties
      ;;                            sidecar-file
      ;;                            (mapcar #'car db-properties)
      ;;                            nil
      ;;                            t)))

      ;; TODO: Add merge option.
      (xmp-file-set-properties sidecar-file db-properties)
      (xmp-sqlite-mod-db-remove-file-properties-all target-file)
      )))

;;;;; Property Change Hook

(defvar xmp-file-property-change-hook nil
  "Hook called when a file's XMP metadata properties change.

Arguments for the function on this hook:
- TARGET-FILE : The file that is described by the metadata.
                Note: This is not the file where metadata is stored.
- PROP-ENAME-ALIST : An alist containing the names of the properties
                     that were changed and their new values.
                     The value is in a format recognized by
                     `xmp-property-element-from'.

This hook is called only when a file property is changed using the
following function:
- `xmp-set-file-property'
- `xmp-set-file-properties'")

(defun xmp-run-file-property-change-hook (target-file event)
  (run-hook-with-args 'xmp-file-property-change-hook target-file event))

;;;;; Remove Properties

(defun xmp-remove-external-file-properties (target-file prop-ename-list-or-all)
  "Remove only externally stored file properties."
  (when (and prop-ename-list-or-all ;; nil means to get nothing
             ;; Sidecar files cannot have external properties
             (not (xmp-sidecar-file-p target-file)))
    (let ((storage-location (xmp-file-property-storage-location target-file)))
      (when (stringp storage-location)
        (xmp-file-merge-db-entry-into-sidecar-file target-file storage-location))

      (cond
       ((eq storage-location 'db)
        (xmp-sqlite-mod-db-remove-file-properties target-file
                                                  prop-ename-list-or-all)
        (xmp-run-file-property-change-hook
         target-file (cons 'remove prop-ename-list-or-all)))
       ((stringp storage-location) ;; sidecar file (not TARGET-FILE)
        (if (eq prop-ename-list-or-all 'all)
            ;; Remove all properties
            (progn
              ;; TODO: Check empty XML file before delete
              (delete-file storage-location)
              (when (xmp-file-cache-enabled nil)
                (xmp-file-cache-remove-file-entry storage-location)))
          ;; Remove properties partially
          (xmp-file-remove-properties storage-location prop-ename-list-or-all))
        (xmp-run-file-property-change-hook
         target-file (cons 'remove prop-ename-list-or-all)))
       (t
        (error "Unknown storage location `%s'" storage-location)))))
  nil)

;;;###autoload
(defun xmp-remove-external-file-metadata (target-file)
  (interactive
   (list (read-file-name (xmp-msg "Target file: "))))
  (xmp-remove-external-file-properties target-file 'all))

;;;###autoload
(defun xmp-remove-external-file-metadata-in-dir (dir)
  (interactive
   (list (read-directory-name (xmp-msg "Directory: "))))
  (dolist (file (xmp-get-external-file-metadata-targets-in-dir dir))
    (xmp-remove-external-file-metadata file)))


;;;;; Write Properties

(defun xmp-set-file-properties (target-file prop-ename-value-alist)
  "Set the XMP properties for TARGET-FILE.

Note that this does not mean writing to TARGET-FILE. Write the metadata
describing TARGET-FILE to the appropriate location.

PROP-ENAME-VALUE-ALIST is an alist of the expanded names and values of
the properties to set. The values must be in a format recognized by
`xmp-property-element-from' and `xmp-pvalue-from'."
  (let ((storage-location (xmp-file-property-storage-location target-file)))
    (when (stringp storage-location)
      (xmp-file-merge-db-entry-into-sidecar-file target-file storage-location))

    (cond
     ((eq storage-location 'db)
      (xmp-sqlite-mod-db-set-file-properties target-file prop-ename-value-alist)
      (xmp-run-file-property-change-hook target-file
                                         (cons 'set prop-ename-value-alist)))
     ((stringp storage-location)
      (xmp-file-set-properties storage-location prop-ename-value-alist)
      (xmp-run-file-property-change-hook target-file
                                         (cons 'set prop-ename-value-alist)))
     (t
      (error "Unknown storage location `%s'" storage-location))))
  prop-ename-value-alist)

(defun xmp-set-file-property (target-file prop-ename value)
  "Set the XMP property PROP-ENAME of TARGET-FILE to VALUE.

Note that this does not mean writing to TARGET-FILE. Write the metadata
describing TARGET-FILE to the appropriate location."
  ;; TODO: Optimize for setting one property
  (xmp-set-file-properties target-file (list (cons prop-ename value)))
  value)


;;;;; Read Properties

(defun xmp-get-file-properties--from-source (source
                                             target-file
                                             prop-ename-list
                                             dst-ns-name-prefix-alist)
  (cond
   ((eq source 'db)
    (xmp-sqlite-mod-db-get-file-properties target-file prop-ename-list
                                           ;; TODO: noerror?
                                           dst-ns-name-prefix-alist))
   ((stringp source)
    (xmp-file-get-properties source prop-ename-list nil t
                             dst-ns-name-prefix-alist))))

(defun xmp-get-file-properties--from-multiple-sources (sources
                                                       target-file
                                                       prop-ename-list-or-all
                                                       dst-ns-name-prefix-alist)
  (cond
   ;; Enumerate all properties
   ((eq prop-ename-list-or-all 'all)
    (let (result)
      (while sources
        (let* ((source (pop sources))
               (props (xmp-get-file-properties--from-source
                       source target-file 'all
                       dst-ns-name-prefix-alist)))
          (setq result (xmp-xml-ename-alist-merge result props))))
      result))
   ;; Get specified properties
   ((listp prop-ename-list-or-all)
    (let ((unloaded-prop-ename-list prop-ename-list-or-all)
          result)
      (while (and unloaded-prop-ename-list sources)
        (let* ((source (pop sources))
               (props (xmp-get-file-properties--from-source
                       source target-file unloaded-prop-ename-list
                       dst-ns-name-prefix-alist)))
          ;; Remove loaded property names from UNLOADED-PROP-ENAME-LIST
          (setq unloaded-prop-ename-list
                (seq-remove (lambda (prop-ename)
                              (xmp-xml-ename-assoc prop-ename props))
                            unloaded-prop-ename-list))
          ;; Merge
          (setq result (nconc result props))))
      result))
   (t
    (error "Invalid prop-ename-list-or-all `%s'" prop-ename-list-or-all))))


(defun xmp-get-file-properties (target-file
                                prop-ename-list-or-all
                                &optional
                                dst-ns-name-prefix-alist)
  "Get the XMP properties of TARGET-FILE.

Note that this does not mean reading from TARGET-FILE. Get the metadata
describing TARGET-FILE from the appropriate location. Use
`xmp-file-get-properties' to read metadata from specified files.

PROP-ENAME-LIST-OR-ALL is a list of the expanded names of the properties
to retrieve or the symbol `all'. nil means to retrieve none. The symbol
`all' means to enumerate all properties. The order or duplication of
properties in the list does not affect the value returned.

DST-NS-NAME-PREFIX-ALIST is the destination for name declarations
encountered during XML parsing. If non-nil, it is treated as a non-empty
list, and the list of (<namespace name> . <namespace prefix>) is
concatenated to the end of it."
  (when prop-ename-list-or-all ;; nil means to get nothing
    (let ((storage-location (xmp-file-property-storage-location target-file)))
      ;; TODO: Do not write files
      (when (stringp storage-location)
        (xmp-file-merge-db-entry-into-sidecar-file target-file storage-location))

      (xmp-get-file-properties--from-multiple-sources
       ;; sources
       (nconc (unless (equal storage-location target-file)
                (list storage-location)) ;; db or sidecar-file
              (list target-file))
       target-file
       prop-ename-list-or-all
       dst-ns-name-prefix-alist))))

(defun xmp-get-external-file-properties (target-file
                                         prop-ename-list-or-all
                                         &optional
                                         dst-ns-name-prefix-alist)
  "Get the external file properties that describe TARGET-FILE."
  (when (and prop-ename-list-or-all ;; nil means to get nothing
             ;; Sidecar files cannot have external properties
             (not (xmp-sidecar-file-p target-file)))
    (let ((storage-location (xmp-file-property-storage-location target-file)))
      ;; TODO: Do not write files
      (when (stringp storage-location)
        (xmp-file-merge-db-entry-into-sidecar-file target-file storage-location))

      (xmp-get-file-properties--from-multiple-sources
       ;; sources
       (list storage-location) ;; db or sidecar-file
       target-file
       prop-ename-list-or-all
       dst-ns-name-prefix-alist))))

(defun xmp-enumerate-file-properties (target-file
                                      &optional
                                      prop-ename-list
                                      dst-ns-name-prefix-alist)
  "Get the XMP properties of TARGET-FILE.

Same as `xmp-get-file-properties', but PROP-ENAME-LIST is optional and
nil means to get all properties."
  (xmp-get-file-properties target-file
                           (or prop-ename-list 'all)
                           dst-ns-name-prefix-alist))

(defun xmp-get-file-property (target-file prop-ename)
  "Get the XMP property PROP-ENAME of TARGET-FILE.

Note that this does not mean reading from TARGET-FILE. Get the metadata
describing TARGET-FILE from the appropriate location."
  (xmp-xml-ename-alist-get ;; TODO: Optimize for retrieving one property
   prop-ename
   (xmp-get-file-properties target-file (list prop-ename))))


;;;;; Move Properties

;;;###autoload
(defun xmp-move-external-file-metadata (old-target-file new-target-file)
  "Change all external file properties that describe OLD-TARGET-FILE to
describe NEW-TARGET-FILE."
  (interactive
   (list (read-file-name (xmp-msg "Old target file: "))
         (read-file-name (xmp-msg "New target file: "))))
  (when (and (not (equal old-target-file new-target-file))
             ;; Sidecar files cannot have external properties
             (not (xmp-sidecar-file-p old-target-file)))

    ;; Note: Only move properties, source and destination may be of
    ;; different location types (e.g. DB->sidecar, sidecar->DB).
    (when-let ((prop-ename-value-alist
                (xmp-get-external-file-properties old-target-file 'all)))
      (xmp-set-file-properties new-target-file prop-ename-value-alist)
      (xmp-remove-external-file-metadata old-target-file))))

;;;###autoload
(defun xmp-move-external-file-metadata-in-dir (old-dir new-dir)
  (interactive
   (list (read-directory-name (xmp-msg "Old directory: "))
         (read-directory-name (xmp-msg "New directory: "))))
  (dolist (old-file (xmp-get-external-file-metadata-targets-in-dir old-dir))
    (xmp-move-external-file-metadata
     old-file
     (file-name-concat new-dir (file-name-nondirectory old-file)))))


;;;;; List Files

(defun xmp-get-external-file-metadata-targets-in-dir (dir)
  (seq-uniq
   (sort
    (nconc
     (xmp-sidecar-file-targets-in-dir dir)
     (xmp-sqlite-mod-db-get-files-in-dir dir))
    :lessp #'string<)
   #'string=))
;; EXAMPLE: (xmp-get-external-file-property-targets-in-dir "~/tmp/")

(defun xmp-get-managed-file-alist-in-dir (dir)
  "Return a list of metadata target files in DIR and their status."
  (let (file-alist)
    ;; Collect
    (cl-loop
     for (fun . key) in '((xmp-sidecar-file-targets-in-dir . :sidecar)
                          (xmp-sqlite-mod-db-get-files-in-dir . :mod-db)
                          (xmp-sqlite-cache-db-get-files-in-dir . :cache-db)
                          (xmp-file-cache-memory-get-files-in-dir . :cache-mem))
     do
     (dolist (file (funcall fun dir))
       (setf (plist-get (alist-get file file-alist nil nil #'string=) key) t)))
    ;; Sort
    (setq file-alist (sort file-alist :key #'car :lessp #'string<))
    ;; Check for stray status
    (cl-loop for file-info in file-alist
             unless (file-exists-p (car file-info))
             do (setf (plist-get (cdr file-info) :stray) t))
    file-alist))

(defun xmp-list-managed-files-in-dir (dir)
  "Display a list of metadata target files in DIR and their status."
  (interactive
   (list (if current-prefix-arg
             (read-directory-name "Directory: ")
           default-directory)))
  (if-let ((file-alist (xmp-get-managed-file-alist-in-dir dir)))
      (let ((buffer (get-buffer-create "*XMP Managed File List*")))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (princ (format (xmp-msg "Dir: %s") dir) buffer)
            (princ "\n" buffer)
            (cl-loop for (file . props) in file-alist
                     do (princ
                         (concat
                          " "
                          (if (plist-get props :stray) "Stray" "     ")
                          " "
                          (if (plist-get props :sidecar) "SC" "  ")
                          " "
                          (if (plist-get props :mod-db) "DB" "  ")
                          " "
                          (if (plist-get props :cache-mem) "MC" "  ")
                          " "
                          (if (plist-get props :cache-db) "DC" "  ")
                          " "
                          file
                          "\n")
                         buffer))
            (goto-char (point-min))
            (setq-local truncate-lines t)
            (view-mode)))
        (pop-to-buffer buffer))
    (message (xmp-msg "No managed files"))))

(provide 'xmp)
;;; xmp.el ends here
