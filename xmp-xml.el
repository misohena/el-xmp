;;; xmp-xml.el --- XML library that supports namespaces -*- lexical-binding: t; -*-

;; Copyright (C) 2024  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: XML

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

;; 1. Parse XML
;;
;;   (setq dom (xmp-xml-parse-file "example.xml"))
;;
;; 2. Retrieve Information
;;
;;   Nodes & Elements:
;;    (xmp-xml-text-node-p dom)
;;    (xmp-xml-element-p dom)
;;    (xmp-xml-element-ename dom) ; `ename' means expanded-name
;;                                ; (ns-keyword . local-name)
;;    (xmp-xml-element-attributes dom)
;;    (xmp-xml-element-children dom)
;;
;;   Expanded Names:
;;    (xmp-xml-ename-ns (xmp-xml-element-ename dom))
;;    (xmp-xml-ename-local (xmp-xml-element-ename dom))
;;    (setq ex-x:xmpmeta
;;          (xmp-xml-ename (xmp-xml-ns-name "adobe:ns:meta/") "xmpmeta"))
;;    (xmp-xml-ename-equal (xmp-xml-element-ename dom) ex-x:xmpmeta)
;;
;;   Attributes:
;;    (xmp-xml-element-attr-value dom (xmp-xml-ename
;;      (xmp-xml-ns-name "http://www.w3.org/XML/1998/namespace") "lang"))
;;    (xmp-xml-attr-ename (xmp-xml-element-attr-find-by-ename dom xmp-xml:lang))
;;    (xmp-xml-attr-value (xmp-xml-element-attr-find-by-ename dom xmp-xml:lang))
;;
;;   Child Nodes:
;;    (nth 2 (xmp-xml-element-children dom))
;;    (setq ex-rdf:Description (xmp-xml-ename
;;      (xmp-xml-ns-name "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
;;      "Description"))
;;    (xmp-xml-element-child-find-by-ename dom ex-rdf:Description)
;;
;; 3. Modify Information
;;
;;   Create Element:
;;    (setq ex-rdf:about
;;          (xmp-xml-ename (xmp-xml-ns-name
;;                           "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
;;                         "about"))
;;    (setq ex-xmp:Rating
;;          (xmp-xml-ename (xmp-xml-ns-name
;;                           "http://purl.org/dc/elements/1.1/")
;;                         "Rating"))
;;    (xmp-xml-element-insert-last
;;      dom
;;      (xmp-xml-element ex-rdf:Description
;;                       (list
;;                         (xmp-xml-attr rdf:about ""))
;;                       (list
;;                         (xmp-xml-element ex-xmp:Rating nil "5"))))
;;
;;   Change Attribute Value:
;;    (setf (xmp-xml-element-attr-value dom xmp-xml:lang) "en")
;;
;;   Remove Attribute:
;;    (xmp-xml-element-attr-remove-by-ename dom ex-xmp:Rating)
;;
;;   Remove Child Node:
;;    (xmp-xml-element-child-remove-by-ename dom ex-xmp:Rating)
;;    (xmp-xml-element-child-remove elem child)
;;
;;   Insert Child Node:
;;    (xmp-xml-element-insert-first elem new-node)
;;    (xmp-xml-element-insert-last elem new-node)
;;    (xmp-xml-element-insert-before elem new-node ref-node)
;;
;; 4. Print XML
;;
;;   (xmp-xml-print (current-buffer) dom nil)
;;
;;   (xmp-xml-write-file "example.xml" dom nil)

;;; Code:

(require 'cl-lib)
(require 'nxml-parse)

;;;; XML Processing

;; Note: Do not use dom.el as it cannot handle expanded-name structures.

;; There is a way to make the representation of the expanded name a
;; single symbol. If we use that method, we can compare expanded names
;; with eq, use assq, or use dom.el. However, this is not currently
;; the case. The expanded name is a cons cell
;; ( <namespace-name-keyword> . <local-name-string> ). Please pay close
;; attention to this when reading the following. It has been carefully
;; implemented so that the method can be switched later.

;;;;; Namespace Name
;; https://www.w3.org/TR/xml-names/#dt-NSName

(defun xmp-xml-ns-name (string)
  "Convert the namespace name STRING to a representation for internal use.

The internal representation is a keyword in the current implementation
using nxml, but it could also be a raw string. Therefore, use the
`equal' function to compare namespace names."
  ;; For nxml-parse-file
  (intern (concat ":" string)))

(defun xmp-xml-ns-name-string (ns)
  "Convert the internal representation of a namespace name NS to a string.
If NS is nil, return nil."
  ;; For nxml-parse-file
  (cond
   ((null ns) nil)
   ((keywordp ns) (substring (symbol-name ns) 1))
   ((symbolp ns) (symbol-name ns))
   ((stringp ns) ns)
   (t (error "Invalid namespace name: %s" ns))))

;;;;; Expanded Name
;; EName : (namespace-name . local-name) | local-name
;; https://www.w3.org/TR/xml-names/#dt-expname

(defsubst xmp-xml-ename (ns local-name)
  "Create an expanded name object.

NS is the namespace name created by `xmp-xml-ns-name' or nil.

LOCAL-NAME is the local name.

In the current implementation, the representation of the expanded-name
is a cons cell containing NS and LOCAL-NAME (however, if NS is nil, it
is a simple string LOCAL-NAME).

In future implementations, it may become a single symbol (or keyword) by
concatenating the NS and LOCAL-NAME."
  ;; The following implementation can use dom.el.
  ;;   (intern (if ns (format "%s::%s" ns local-name) local-name))
  ;; For nxml-parse-file
  (if ns
      (cons ns local-name)
    local-name))

(defsubst xmp-xml-ename-ns (ename)
  "Return the namespace name of the expanded name ENAME.
Return nil if there is no namespace name.

The return value may be a symbol (keyword) or a string depending on the
implementation. Use `equal' to compare. To ensure conversion to a
string, use `xmp-xml-ns-name-string'."
  (when (consp ename)
    (car ename)))

(defsubst xmp-xml-ename-local (ename)
  "Return the local name string of the expanded name ENAME."
  (if (consp ename)
      (cdr ename)
    ename))

(defun xmp-xml-ename-equal (ename1 ename2)
  "Return non-nil if the expanded names ENAME1 and ENAME2 are the same."
  ;; Is there a case like the following?
  ;;   (xmp-xml-ename-equal '(nil . "local-name") "local-name")
  ;; If not, just use `equal'.
  ;;   (equal ename1 ename2)
  ;; Of course, if expanded name can be represented by a single
  ;; symbol, just `eq' is sufficient.
  (and (equal (xmp-xml-ename-ns ename1) (xmp-xml-ename-ns ename2))
       (equal (xmp-xml-ename-local ename1) (xmp-xml-ename-local ename2))))

(defun xmp-xml-ename< (ename1 ename2)
  "Return non-nil if ENAME1 is less than ENAME2 in lexicographic order."
  (let ((ns1 (xmp-xml-ns-name-string (xmp-xml-ename-ns ename1)))
        (ns2 (xmp-xml-ns-name-string (xmp-xml-ename-ns ename2))))
    (if ns1
        (if ns2
            (or (string< ns1 ns2)
                (and (not (string< ns2 ns1))
                     (string< (xmp-xml-ename-local ename1)
                              (xmp-xml-ename-local ename2))))
          ;; "" < nil
          nil)
      (if ns2
          ;; nil < ""
          t
        (string< (xmp-xml-ename-local ename1)
                 (xmp-xml-ename-local ename2))))))

(defmacro xmp-xml-ename-alist-get (ename-key ename-alist
                                             &optional default remove)
  "Apply `alist-get' to the alist ENAME-ALIST whose keys are expanded-names.

ENAME-KEY is the expanded name that is the key. It can be created with
`xmp-xml-ename'."
  `(alist-get ,ename-key ,ename-alist ,default ,remove #'xmp-xml-ename-equal))

(defmacro xmp-xml-ename-assoc (ename-key ename-alist)
  "Apply `assoc' to the alist ENAME-ALIST whose keys are expanded-names.

ENAME-KEY is the expanded name that is the key. It can be created with
`xmp-xml-ename'."
  `(assoc ,ename-key ,ename-alist #'xmp-xml-ename-equal))

(defun xmp-xml-ename-alist-sort (ename-alist)
  "Sort the alist ENAME-ALIST whose keys are expanded names."
  (sort ename-alist :lessp
        (lambda (name-pvalue1 name-pvalue2)
          (xmp-xml-ename< (car name-pvalue1)
                          (car name-pvalue2)))))

(defun xmp-xml-ename-alist-merge (primary-ename-alist secondary-ename-alist)
  "Merge the two alists PRIMARY-ENAME-ALIST and SECONDARY-ENAME-ALIST,
whose keys are expanded-name."
  (let ((l-alist (reverse primary-ename-alist))) ;; reverse & copy
    (dolist (r secondary-ename-alist)
      (unless (assoc (car r) l-alist #'xmp-xml-ename-equal)
        (push r l-alist)))
    (nreverse l-alist)))
;; TEST: (xmp-xml-ename-alist-merge (list (cons xmp-xmp:Label "LABEL1") (cons xmp-xmp:Rating "5")) (list (cons xmp-xmp:Rating "0") (cons xmp-xmp:CreateDate "2024-10-03"))) => (((:http://ns.adobe.com/xap/1.0/ . "Label") . "LABEL1") ((:http://ns.adobe.com/xap/1.0/ . "Rating") . "5") ((:http://ns.adobe.com/xap/1.0/ . "CreateDate") . "2024-10-03"))

(defun xmp-xml-ename-member (ename list)
  "Return non-nil if ENAME is an element of LIST.
Comparison done with `xmp-xml-ename-equal'.
The value is actually the tail of LIST whose car is ENAME."
  ;; (member ename list)
  (while (and list (not (xmp-xml-ename-equal (car list) ename)))
    (setq list (cdr list)))
  list)

(defun xmp-xml-ename-clone (ename)
  "Duplicate the expanded name ENAME.

Use of this function is discouraged. If you are changing only part of an
expanded name, it is better to create a new expanded name object using
`xmp-xml-ename'."
  (xmp-xml-ename (xmp-xml-ename-ns ename) (xmp-xml-ename-local ename)))


;;;;; Names defined in the XML specification

;; https://www.w3.org/TR/xml-names/
;; https://www.w3.org/XML/1998/namespace

(defconst xmp-xmlns:
  (xmp-xml-ns-name "http://www.w3.org/2000/xmlns/")
  "The internal representation of the namespace name
\"http://www.w3.org/2000/xmlns/\".")

(defconst xmp-xml:
  (xmp-xml-ns-name "http://www.w3.org/XML/1998/namespace")
  "The internal representation of the namespace name
\"http://www.w3.org/XML/1998/namespace\".")

(defconst xmp-xml:lang (xmp-xml-ename xmp-xml: "lang"))
(defconst xmp-xml:space (xmp-xml-ename xmp-xml: "space"))
(defconst xmp-xml:base (xmp-xml-ename xmp-xml: "base"))
(defconst xmp-xml:id (xmp-xml-ename xmp-xml: "id"))


;;;;; Node
;; Node : TextNode | Element
;; Element : (EName (Attribute...) . (Node...))

;;;;;; Text Node

(defsubst xmp-xml-text-node-p (node)
  "Return non-nil if NODE is a text node."
  (stringp node))

;;;;;; Element

(defun xmp-xml-element (ename &optional attributes children)
  "Create a new element.

ENAME is the element expanded name.
ATTRIBUTES is a list of attributes.
CHILDREN is a list of child nodes.

ENAME, ATTRIBUTES, and CHILDREN are used directly on the created element
object; copy them before the call if necessary."
  (cons ename (cons attributes children)))

(defsubst xmp-xml-element-p (node)
  "Return non-nil if NODE is an element."
  (consp node))

(defmacro xmp-xml-element-ename (node)
  "Return the element name of NODE as a expanded name.
This macro can be used as a generalized variable."
  `(car ,node))

(defmacro xmp-xml-element-attributes (node)
  "Return the list of attributes for NODE.
Each attribute is in the format (expanded name . value).
This macro can be used as a generalized variable."
  `(cadr ,node))

(defmacro xmp-xml-element-children (node)
  "Return the list of children of NODE.
Each child is either an element or a text node.
This macro can be used as a generalized variable."
  `(cddr ,node))

(defmacro xmp-xml-element-children--prev (node)
  "Return the cons cell in NODE whose cdr is the list of NODE's children."
  `(cdr ,node))

;;;;; Element Name

(defun xmp-xml-element-enamed-p (node ename)
  "Return non-nil if NODE is an element and its expanded-name
matches ENAME."
  (and (xmp-xml-element-p node)
       (xmp-xml-ename-equal (xmp-xml-element-ename node) ename)))

(defun xmp-xml-element-named-p (node ns local-name)
  "Return non-nil if NODE is an element and its expanded-name
matches NS and LOCAL-NAME."
  (and (xmp-xml-element-p node)
       (equal (xmp-xml-ename-ns (xmp-xml-element-ename node)) ns)
       (equal (xmp-xml-ename-local (xmp-xml-element-ename node)) local-name)))

;;;;; Attribute
;; Attribute : (EName . AttributeValue)

(defun xmp-xml-attr (ename value)
  "Creates an attribute object.
The expanded-name of the attribute is ENAME.
The value of the attribute is VALUE."
  (cons ename value))

(defmacro xmp-xml-attr-ename (attr)
  "Return the expanded name of attribute ATTR.
This macro can be used as a generalized variable."
  `(car ,attr))

(defmacro xmp-xml-attr-value (attr)
  "Return the value of attribute ATTR.
This macro can be used as a generalized variable."
  `(cdr ,attr))

(defun xmp-xml-attr-enamed-p (attr ename)
  "Return non-nil if the expanded name of ATTR matches ENAME."
  (xmp-xml-ename-equal (xmp-xml-attr-ename attr) ename))

;;;;; Namespace Declaration
;; https://www.w3.org/TR/xml-names/#dt-NSDecl

(defun xmp-xml-attr-nsdecl-p (attr)
  "Return non-nil if ATTR is a namespace declaration
(xmlns:<prefix>= or xmlns=)."
  (equal (xmp-xml-ename-ns (xmp-xml-attr-ename attr)) xmp-xmlns:))

(defun xmp-xml-attr-default-nsdecl-p (attr)
  "Return non-nil if ATTR is a default namespace declaration (xmlns=)."
  (and (xmp-xml-attr-nsdecl-p attr)
       (member (xmp-xml-ename-local (xmp-xml-attr-ename attr))
               '(nil
                 ""
                 ;; For nxml-parse-file
                 ;; Note: `nxml-parse-file' generates the expanded
                 ;; name (:http://www.w3.org/2000/xmlns/ . "xmlns")
                 ;; for xmlns=.
                 "xmlns"))))

(defun xmp-xml-attr-as-decl (attr)
  "Return the contents of the namespace declaration indicated by ATTR.

Return a cons cell where car is the namespace name and cdr is the
prefix (or nil, for the default namespace).

ATTR must be one for which `xmp-xml-attr-nsdecl-p' returns non-nil."
  (when (xmp-xml-attr-nsdecl-p attr)
    (cons (xmp-xml-ns-name (xmp-xml-attr-value attr))
          (if (xmp-xml-attr-default-nsdecl-p attr)
              nil ;; default namespace
            (xmp-xml-ename-local (xmp-xml-attr-ename attr))))))

;;;;; Element Attributes

(defmacro xmp-xml-element-attr-value (elem ename)
  "Return the value of the attribute identified by expanded-name ENAME of
element ELEM.

ELEM must be an element (`xmp-xml-element-p').

This macro can be used as a generalized variable."
  `(alist-get ,ename
              (xmp-xml-element-attributes ,elem)
              nil nil #'xmp-xml-ename-equal))

;; I commented out the following because the argument might be
;; mistaken for an attribute name.
;; (defun xmp-xml-element-attr-remove (elem attr-cell)
;;   (setf (xmp-xml-element-attributes elem)
;;         (delq attr-cell (xmp-xml-element-attributes elem))))

(defun xmp-xml-element-attr-remove-by-ename (elem ename)
  "Remove the attribute of the element ELEM whose name matches ENAME."
  (setf (alist-get ename (xmp-xml-element-attributes elem)
                   nil t #'xmp-xml-ename-equal)
        nil))

(defun xmp-xml-element-attr-find-by-ename (elem ename)
  "Return the attribute of the element ELEM whose name matches ENAME."
  (seq-find (lambda (attr) (xmp-xml-ename-equal (xmp-xml-attr-ename attr)
                                                 ename))
            (xmp-xml-element-attributes elem)))

(defun xmp-xml-element-attributes-exclude (elem enames)
  "Return all attributes of the element ELEM, excluding those whose attribute
name matches any element in the list ENAMES."
  (seq-remove (lambda (attr)
                (xmp-xml-ename-member
                 (xmp-xml-attr-ename attr)
                 enames))
              (xmp-xml-element-attributes elem)))

(defun xmp-xml-element-attributes-without-nsdecl (elem)
  "Return the attributes of the element ELEM, excluding any that represent
namespace declarations."
  (seq-remove #'xmp-xml-attr-nsdecl-p (xmp-xml-element-attributes elem)))

(defun xmp-xml-element-attributes-count-without-nsdecl (elem)
  "Return the number of attributes that element ELEM has, excluding any
that represent namespace declarations."
  (cl-count-if-not #'xmp-xml-attr-nsdecl-p (xmp-xml-element-attributes elem)))


;;;;; Element Children

(defun xmp-xml-element-child-remove (elem child)
  "Remove the child node CHILD from the element ELEM."
  (setf (xmp-xml-element-children elem)
        (delq child (xmp-xml-element-children elem))))

(defun xmp-xml-element-child-remove-by-ename (elem ename)
  "Remove all elements whose expanded name is ENAME from the element ELEM."
  (setf (xmp-xml-element-children elem)
        (cl-remove-if (lambda (child) (xmp-xml-element-enamed-p child ename))
                      (xmp-xml-element-children elem))))

(defun xmp-xml-element-child-find-by-ename (elem ename)
  "Find and return the first element in the element ELEM whose expanded
name is ENAME."
  (seq-find (lambda (child)
              (xmp-xml-element-enamed-p child ename))
            (xmp-xml-element-children elem)))

(defun xmp-xml-element-insert-before (elem new-node reference-node)
  "Add NEW-NODE before REFERENCE-NODE, which is a child of the element ELEM.
If REFERENCE-NODE is nil, NEW-NODE will be the first child of ELEM."
  (let ((prev-cell (xmp-xml-element-children--prev elem)))
    (while (and (cdr prev-cell)
                (not (eq (cadr prev-cell) reference-node)))
      (setq prev-cell (cdr prev-cell)))
    (setcdr prev-cell (cons new-node (cdr prev-cell))))
  new-node)

(defun xmp-xml-element-insert-last (elem new-node)
  "Add NEW-NODE as the last child of the element ELEM."
  (setcdr (last (xmp-xml-element-children--prev elem))
          (cons new-node nil))
  new-node)

(defun xmp-xml-element-insert-first (elem new-node)
  "Add NEW-NODE as the first child of the element ELEM."
  (setf (xmp-xml-element-children elem)
        (cons new-node (xmp-xml-element-children elem)))
  new-node)

(defun xmp-xml-element-children-text (node)
  "Return a single string by concatenating all the text nodes that are
children of NODE.

Return nil if NODE is not an element or has no text nodes as children.

Grandchild nodes are not included."
  (when (xmp-xml-element-p node)
    (let ((text nil)) ;; Return nil if there are no text nodes in NODE.
      (dolist (child (xmp-xml-element-children node))
        (when (xmp-xml-text-node-p child)
          (setq text (concat text child))))
      text)))

;;;;; Parser

(defun xmp-xml-parse-file (file)
  "Parse an XML FILE and return a DOM tree."
  (xmp-xml-remove-spaces
   ;; This won't work because there is a bug in attribute namespace expansion.
   ;;(car (xml-parse-file file nil t))
   (nxml-parse-file
    ;; Note: expand-file-name is required because the
    ;; default-directory of the buffer used by nxml-parse-file may be
    ;; different from the one before the call.
    (expand-file-name file))))

(defun xmp-xml-parse-bytes (bytes &optional filename)
  "Parse XML from raw BYTES that have not been decoded in character encoding.

If there is an XML declaration at the beginning of the BYTES,
the character encoding is identified from the encoding= specification
( <?xml version=\"1.0\" encoding=\"????\"?> ). If there is no XML
declaration, it is automatically determined from the character usage and
FILENAME.

If you already know that the BYTES is encoded in UTF-8,
use (xmp-xml-parse-string (decode-coding-string bytes \\='utf-8))."
  (xmp-xml-remove-spaces
   (with-temp-buffer
     (insert bytes)
     (let ((set-auto-coding-function 'nxml-set-xml-coding))
       (decode-coding-inserted-region (point-min) (point-max) (or filename "")))
     (goto-char (point-min))
     (nxml-parse-instance))))

(defun xmp-xml-parse-string (string)
  "Parse XML from STRING.

STRING must be a string after character encoding has been decoded."
  (xmp-xml-remove-spaces
   (save-excursion
     (with-temp-buffer
       (insert string)
       (goto-char (point-min))
       (nxml-parse-instance)))))


;;;;; Spaces

(defun xmp-xml-blank-p (node)
  "Return non-nil if NODE is a text node that consists only of whitespace
characters."
  (and (xmp-xml-text-node-p node)
       (string-blank-p node)))

(defun xmp-xml-remove-spaces (node)
  "Remove whitespace from the entire NODE tree."
  ;; TODO: Support xml:space=
  (cond
   ((xmp-xml-element-p node)
    (setf (xmp-xml-element-children node)
          (cl-loop for child in (xmp-xml-element-children node)
                   unless (xmp-xml-blank-p child)
                   collect (xmp-xml-remove-spaces child)))
    node)
   ((xmp-xml-text-node-p node)
    (string-trim node))
   (t
    node)))


;;;;; Namespace

(defun xmp-xml-standard-ns-name-prefix-alist ()
  "Return an alist of standard namespace names and prefixes as defined in
the XML specification.

Return a newly created list that can be concatenated with `nconc'."
  ;; a new list must be created
  (list (cons xmp-xml: "xml")
        (cons xmp-xmlns: "xmlns")))

(defun xmp-xml-move-nsdecls-to-root (node)
  "Move all namespace declarations in the NODE tree to the root node.

Namespace declarations in irregular locations can cause bugs. To avoid
this problem, this function is used to consolidate namespace
declarations to the root as a precaution."
  (when (xmp-xml-element-p node)
    (let ((ns-name-prefix-alist (xmp-xml-collect-nsdecls node)))
      (xmp-xml-remove-nsdecls node)
      ;; TODO: Remove duplicate namespace names
      ;;  ((http:ns1/ . "ns-a") (http:ns1/ . "ns-b"))
      (xmp-xml-insert-nsdecls node ns-name-prefix-alist)))
  node)

(defun xmp-xml-collect-nsdecls (node)
  "Collect namespace declarations (xmlns:??= or xmlns=) used in the NODE tree.
Return a list of (<namespace-name> . <prefix>)."
  (when (xmp-xml-element-p node)
    (seq-uniq (xmp-xml-collect-nsdecls-non-unique node))))

(defun xmp-xml-collect-nsdecls-non-unique (node)
  "Collect namespace declarations (xmlns:??= or xmlns=) used in the NODE tree.
Return a list of (<namespace-name> . <prefix>)."
  (when (xmp-xml-element-p node)
    (nconc
     ;; Collect from the NODE
     (cl-loop for attr in (xmp-xml-element-attributes node)
              when (xmp-xml-attr-nsdecl-p attr)
              collect (xmp-xml-attr-as-decl attr))
     ;; Collect from the descendants of NODE
     (cl-loop for child in (xmp-xml-element-children node)
              nconc (xmp-xml-collect-nsdecls-non-unique child)))))

(defun xmp-xml-remove-nsdecls (node)
  "Remove all namespace declarations (xmlns:?? or xmlns=)."
  (when (xmp-xml-element-p node)
    (setf (xmp-xml-element-attributes node)
          (seq-remove #'xmp-xml-attr-nsdecl-p (xmp-xml-element-attributes node)))
    (dolist (child (xmp-xml-element-children node))
      (xmp-xml-remove-nsdecls child)))
  node)

(defun xmp-xml-insert-nsdecl (node ns prefix)
  "Add a namespace declaration NS and PREFIX to NODE."
  (when (xmp-xml-element-p node)
    (unless (member prefix '("xml" "xmlns"))
      (setf (xmp-xml-element-attributes node)
            (cons
             (xmp-xml-attr (xmp-xml-ename xmp-xmlns: prefix)
                           (xmp-xml-ns-name-string ns))
             (xmp-xml-element-attributes node)))))
  node)

(defun xmp-xml-insert-nsdecls (node ns-name-prefix-alist)
  "Add a namespace declarations NS-NAME-PREFIX-ALIST to NODE."
  (when (xmp-xml-element-p node)
    (dolist (nsdecl ns-name-prefix-alist)
      (xmp-xml-insert-nsdecl node (car nsdecl) (cdr nsdecl))))
  node)

(defun xmp-xml-collect-used-ns (node &optional used-ns-list)
  "Return a list of namespace names used in the NODE tree."
  (when (xmp-xml-element-p node)
    (when-let ((ns (xmp-xml-ename-ns (xmp-xml-element-ename node))))
      (unless (member ns used-ns-list)
        (push ns used-ns-list)))

    (dolist (attr (xmp-xml-element-attributes node))
      (unless (xmp-xml-attr-nsdecl-p attr)
        (when-let ((ns (xmp-xml-ename-ns (xmp-xml-attr-ename attr))))
          (unless (member ns used-ns-list)
            (push ns used-ns-list)))))

    (dolist (child (xmp-xml-element-children node))
      (setq used-ns-list (xmp-xml-collect-used-ns child used-ns-list))))
  used-ns-list)


;;;;; XML Print

(defvar xmp-xml-no-line-break nil)
(defvar xmp-xml-indent-offset 2)

(defun xmp-xml-escape-chars (str)
  (replace-regexp-in-string
   "\\([\"&<\r\n]\\)"
   (lambda (str)
     (pcase (elt str 0)
       (?\" "&quot;")
       (?& "&amp;")
       (?< "&lt;")
       ;; TODO: Is it better not to escape the next character in a text node?
       (?\n "&#10;")
       (?\r "&#13;")))
   str
   t t))

(defun xmp-xml-ns-name-to-prefix (ns ns-name-prefix-alist &optional noerror)
  "Convert namespace name NS (URI) to prefix using NS-NAME-PREFIX-ALIST."
  (if (equal ns xmp-xml:)
      "xml" ;; Reserved
    (if-let ((cell (assoc ns ns-name-prefix-alist)))
        (cdr cell)
      (unless noerror
        (error "Namespace `%s' is not defined" ns)))))

(defun xmp-xml-element-name-string (node ns-name-prefix-alist)
  (let* ((ename (xmp-xml-element-ename node))
         (local-name (xmp-xml-ename-local ename)))
    (if-let* ((ns (xmp-xml-ename-ns ename))
              (prefix (xmp-xml-ns-name-to-prefix ns ns-name-prefix-alist)))
        (concat prefix ":" local-name)
      local-name)))

(defun xmp-xml-attr-name-string (attr ns-name-prefix-alist)
  (let ((ename (xmp-xml-attr-ename attr)))
    (cond
     ;; Namespace declaration
     ((xmp-xml-attr-nsdecl-p attr)
      (if (xmp-xml-attr-default-nsdecl-p attr)
          ;; Default
          "xmlns"
        ;; Prefixed
        (concat "xmlns" ":" (xmp-xml-ename-local ename))))
     ;; Prefixed
     ((when-let ((ns (xmp-xml-ename-ns ename)))
        (let ((prefix (xmp-xml-ns-name-to-prefix ns ns-name-prefix-alist)))
          ;; TODO:
          (unless prefix
            (error "Unable to get prefix for namespace name %s in attr %s"
                   ns (xmp-xml-ename-local ename)))
          (concat prefix ":" (xmp-xml-ename-local ename)))))
     ;; Unprefixed
     (t
      (xmp-xml-ename-local ename)))))

(defun xmp-xml-print-attributes (stream node ns-name-prefix-alist indent)
  (dolist (attr (xmp-xml-element-attributes node))
    ;;(princ " ")
    (xmp-xml-print-indent-or-space stream (+ indent 2))
    (princ (concat
            (xmp-xml-attr-name-string attr ns-name-prefix-alist)
            "="
            "\""
            (xmp-xml-escape-chars (xmp-xml-attr-value attr))
            "\"")
           stream)))

(defun xmp-xml-print-indent-or-space (stream indent)
  (if xmp-xml-no-line-break
      (princ " " stream)
    (xmp-xml-print-indent stream indent)))

(defun xmp-xml-print-indent (stream indent)
  (unless xmp-xml-no-line-break
    (princ "\n" stream)
    (princ (make-string (* indent xmp-xml-indent-offset) ? ) stream)))

(defun xmp-xml-print-element (stream node ns-name-prefix-alist indent)
  (princ "<" stream)
  (princ (xmp-xml-element-name-string node ns-name-prefix-alist) stream)
  (xmp-xml-print-attributes stream node ns-name-prefix-alist indent)

  (if-let ((children (xmp-xml-element-children node)))
      (let ((mline (xmp-xml-element-p (car children))))
        (princ ">" stream)

        (dolist (child children)
          (when mline
            (xmp-xml-print-indent stream (1+ indent)))
          (xmp-xml-print-node stream child ns-name-prefix-alist (1+ indent)))
        (when mline
          (xmp-xml-print-indent stream indent))
        (princ "</" stream)
        (princ (xmp-xml-element-name-string node ns-name-prefix-alist) stream)
        (princ ">" stream))
    (princ "/>" stream)))

(defun xmp-xml-print-text-node (stream node)
  (princ (xmp-xml-escape-chars node) stream))

(defun xmp-xml-print-node (stream node ns-name-prefix-alist indent)
  (if (xmp-xml-element-p node)
      (xmp-xml-print-element stream node ns-name-prefix-alist indent)
    (xmp-xml-print-text-node stream node)))

(defun xmp-xml-print-text-decl (stream)
  (princ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" stream))

(defun xmp-xml-print (stream node &optional ns-name-prefix-alist no-xml-decl)
  ;; Prepare for destructive operations.
  (setq node (copy-tree node))

  ;; Append declarations existing in the NODE tree to alist.
  (setq ns-name-prefix-alist
        (append ns-name-prefix-alist
                (xmp-xml-collect-nsdecls node)))

  ;; Create declarations for the namespaces that are actually used.
  (let ((used-prefixes nil)
        (prefix-number 0))
    (setq ns-name-prefix-alist
          (mapcar (lambda (ns)
                    (let* ((decl (assoc ns ns-name-prefix-alist))
                           ;; Note: prefix=nil means default namespace.
                           (prefix (if decl (cdr decl) 'undecided)))
                      ;; Determine the prefix.
                      (while (or
                              ;; The declaration does not yet exist.
                              (equal prefix 'undecided)
                              ;; The prefix is ​​already in use.
                              (member prefix used-prefixes))
                        ;; Generate new prefix
                        (setq prefix (format "ns%d" (cl-incf prefix-number))))

                      (push prefix used-prefixes)
                      (cons ns prefix)))
                  (nreverse (xmp-xml-collect-used-ns node)))))

  ;; Place namespace declarations only in the root element.
  (xmp-xml-remove-nsdecls node)
  (xmp-xml-insert-nsdecls node (reverse ns-name-prefix-alist))

  ;; Print document.
  (unless no-xml-decl
    (xmp-xml-print-text-decl stream))
  (xmp-xml-print-node stream node ns-name-prefix-alist 0)
  (unless xmp-xml-no-line-break
    (princ "\n" stream))
  t)

(defun xmp-xml-write-file (file node &optional ns-name-prefix-alist)
  "Write the NODE tree to FILE in XML.

NS-NAME-PREFIX-ALIST is an alist that represents the correspondence
between namespace names and namespace prefixes. If the same namespace
name is specified multiple times in NS-NAME-PREFIX-ALIST, the first one
is used. If a namespace name that is not specified in
NS-NAME-PREFIX-ALIST is used in NODE, the prefix ns<n>: (where <n> is a
sequential number) is used."
  (with-temp-file file
    (xmp-xml-print (current-buffer) node ns-name-prefix-alist)
    (set-buffer-file-coding-system 'utf-8)))

(provide 'xmp-xml)
;;; xmp-xml.el ends here
