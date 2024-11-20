;;; xmp-test-value-types.el ---                     -*- lexical-binding: t; -*-

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

;; (pp (xmp-file-get-properties "xmp-test-value-types.xmp" 'all t) (current-buffer))

(ert-deftest xmp--test-value-types ()
  (let ((pvalues (xmp-file-get-properties "xmp-test-value-types.xmp" 'all nil t))
        (expected
         '(
           ;;
           ;; Simple(Text)
           ;;

           ((:http://misohena.jp/ns1/ . "SimpleProp1") :pv-type text
            :value "SimpleValue")

           ((:http://misohena.jp/ns1/ . "SimpleProp2") :pv-type text
            :value "" :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "SimpleProp3") :pv-type text
            :value "SimpleValue"
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ((:http://misohena.jp/ns1/ . "SimpleProp4") :pv-type text
            :value "SimpleValue"
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "SimpleProp5") . nil)
           ;; ((:http://misohena.jp/ns1/ . "SimpleProp5") :pv-type struct
           ;;  :value
           ;;  (((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
           ;;    :value "Qual1Val"))
           ;;  :qualifiers
           ;;  (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
           ;;    :value "ja")))

           ((:http://misohena.jp/ns1/ . "SimpleProp6") :pv-type text
            :value "SimpleValue"
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ((:http://misohena.jp/ns1/ . "SimpleProp7") :pv-type text
            :value "SimpleValue"
            :qualifiers
            (((:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "type") :pv-type uri
              :value "http://misohena.jp/ns1/SimpleType")
             ((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ((:http://misohena.jp/ns1/ . "SimpleProp8") :pv-type text
            :value "SimpleValue"
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ;;
           ;; Simple(URI)
           ;;

           ((:http://misohena.jp/ns1/ . "URIProp1") :pv-type uri
            :value "URIValue"
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ((:http://misohena.jp/ns1/ . "URIProp2") :pv-type uri
            :value "URIValue"
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ((:http://misohena.jp/ns1/ . "URIProp3") :pv-type uri
            :value "URIValue"
            :qualifiers
            (((:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "type") :pv-type uri
              :value "http://misohena.jp/ns1/URIType")
             ((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ((:http://misohena.jp/ns1/ . "URIProp4") :pv-type uri
            :value "URIValue" :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ;;
           ;; Struct
           ;;

           ((:http://misohena.jp/ns1/ . "StructProp1") :pv-type struct
            :value
            (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
              :value "Field1Val")
             ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
              :value "Field2Val"))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "StructProp2") :pv-type struct
            :value
            (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
              :value "Field1Val")
             ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
              :value "Field2Val"))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "StructProp3") :pv-type struct
            :value
            (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
              :value "Field1Val")
             ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
              :value "Field2Val"))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ((:http://misohena.jp/ns1/ . "StructProp4") :pv-type struct
            :value
            (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
              :value "Field1Val")
             ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
              :value "Field2Val"))
            :qualifiers
            (;; typeはQualifierにならないとダメらしい。
             ((:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "type") :pv-type uri
              :value "http://misohena.jp/ns1/StructType")
             ((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "StructProp5") :pv-type struct
            :value
            (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
              :value "Field1Val")
             ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
              :value "Field2Val"))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")))

           ((:http://misohena.jp/ns1/ . "StructProp6") :pv-type struct
            :value
            (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
              :value "Field1Val")
             ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
              :value "Field2Val"))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ;;
           ;; Array
           ;;

           ((:http://misohena.jp/ns1/ . "ArrayProp1") :pv-type array
            :value
            ((:pv-type
              text :value "BagItem1Val" :qualifiers
              (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
                :value "ja")))
             (:pv-type
              text
              :value "BagItem2Val" :qualifiers
              (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
                :value "ja"))))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja"))
            :array-type (:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "Bag"))

           ((:http://misohena.jp/ns1/ . "ArrayProp2") :pv-type array
            :value
            ((:pv-type
              text
              :value "AltItem1Val" :qualifiers
              (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
                :value "ja")))
             (:pv-type
              text
              :value "AltItem2Val" :qualifiers
              (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
                :value "ja"))))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja"))
            :array-type (:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "Alt"))

           ((:http://misohena.jp/ns1/ . "ArrayProp3") :pv-type array
            :value
            ((:pv-type
              text
              :value "SeqItem1Val" :qualifiers
              (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
                :value "ja")))
             (:pv-type
              text
              :value "SeqItem2Val" :qualifiers
              (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
                :value "ja"))))
            :array-type (:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "Seq")
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja")
             ((:http://misohena.jp/ns1/ . "Qual1") :pv-type text
              :value "Qual1Val")))

           ((:http://misohena.jp/ns1/ . "ArrayProp4") :pv-type array
            :value
            ((:pv-type
              struct
              :value
              (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
                :value "Field1-1Val")
               ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
                :value "Field1-2Val"))
              :qualifiers
              (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
                :value "ja")))
             (:pv-type
              struct :value
              (((:http://misohena.jp/ns1/ . "Field1") :pv-type text
                :value "Field2-1Val")
               ((:http://misohena.jp/ns1/ . "Field2") :pv-type text
                :value "Field2-2Val"))
              :qualifiers
              (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
                :value "ja"))))
            :qualifiers
            (((:http://www.w3.org/XML/1998/namespace . "lang") :pv-type text
              :value "ja"))
            :array-type (:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "Bag")))))

    (dolist (exp expected)
      (let ((prop-name (car exp))
            (exp-value (cdr exp)))
        (should (equal
                 (alist-get prop-name pvalues nil nil #'xmp-xml-ename-equal)
                 exp-value)
                )))))
