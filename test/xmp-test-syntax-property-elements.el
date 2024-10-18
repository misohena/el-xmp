;;; xmp-test-syntax-property-elements.el ---        -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'ert)
(require 'xmp)

;; (pp (xmp-file-enumerate-properties "xmp-test-syntax-property-elements.xmp" nil t) (current-buffer))

(ert-deftest xmp--test-syntax-property-elements ()
  (let ((pvalues (xmp-file-enumerate-properties "xmp-test-syntax-property-elements.xmp" nil nil t))
        (expected
         '(
           ;;
           ;; Property Attributes
           ;;

           ((:http://misohena.jp/ns1/ . "PropAttr1") :pv-type text
            :value "PropAttr1Val")

           ((:http://misohena.jp/ns1/ . "PropAttr2") :pv-type text
            :value "PropAttr2Val")

           ;;
           ;; Empty Property Element
           ;;

           ;; Note: It's unclear how to handle empty values; the SDK
           ;; treats them as if the property doesn't exist.
           ((:http://misohena.jp/ns1/ . "EmptyPropElt1") :pv-type text
            :value ""
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "EmptyPropElt2") :pv-type text
            :value "EmptyPropElt2Val"
            :qualifiers
            (((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Value")
             ((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "EmptyPropElt3") :pv-type uri
            :value "https://misohena.jp/EmptyPropElt3URI"
            :qualifiers
            (((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Value")
             ((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "EmptyPropElt4") :pv-type struct
            :value
            (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
              :value "Field1Val")
             ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
              :value "Field2Val"))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ;;
           ;; Literal Property Element
           ;;

           ((:http://misohena.jp/ns1/ . "LiteralPropElt1") :pv-type text
            :value "LiteralPropElt1Val")

           ((:http://misohena.jp/ns1/ . "LiteralPropElt2") :pv-type text
            :value "LiteralPropElt1Val"
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ;;
           ;; Resource Property Element
           ;;

           ((:http://misohena.jp/ns1/ . "ResPropElt1") :pv-type struct
            :value
            (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
              :value "ResPropElt1Field1Val")
             ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
              :value "ResPropElt2Field1Val"))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "ResPropElt2") :pv-type text
            :value "ResPropElt2Val" :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ((:http://misohena.jp/ns1/ . "ResPropElt3") :pv-type array
            :value
            ((:pv-type text :value "ResPropElt3Item1")
             (:pv-type text :value "ResPropElt3Item2"))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja"))
            :array-type (:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "Bag"))

           ((:http://misohena.jp/ns1/ . "ResPropElt4") :pv-type struct
            :value
            (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
              :value "Field1Val")
             ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
              :value "Field2Val"))
            :qualifiers
            (((:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "type") :pv-type uri
              :value "http://misohena.jp/ns1/Type")
             ((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ;;
           ;; parseType="Resource" Property Element
           ;;

           ((:http://misohena.jp/ns1/ . "ParseTypeResourcePropElt1") :pv-type
            struct
            :value
            (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
              :value "PTRPropElt1Field1Val")
             ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
              :value "PTRPropElt1Field2Val"))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "ParseTypeResourcePropElt2") :pv-type text
            :value "PTRPropElt2Val"
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ((:http://misohena.jp/ns1/ . "ParseTypeResourcePropElt3") :pv-type uri
            :value "URIValue"
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val"))))))

    (dolist (exp expected)
      (let ((prop-name (car exp))
            (exp-value (cdr exp)))
        (should (equal
                 (alist-get prop-name pvalues nil nil #'xmp-xml-ename-equal)
                 exp-value))))))
