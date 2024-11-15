;;; xmp-sqlite.el ---                                -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'sqlite)
(require 'xmp)

;;;; Object Property Database

;; Database that can store objects with any properties

;; Tables:
;; - object_property_values : object_id, property_id, value
;; - properties : id, name
;; - elxmp_db_info : key, value

;; Example:
;;   elxmp_db_info
;;     key             value
;;     ---------------+----
;;     version         #x100
;;     last_object_id  0      (See:`xmp-sqlite-odb-new-object-id')

;;   properties
;;     id  name
;;     ---+------------
;;     1   elxmp://el-xmp/xmlns/:ObjectType
;;     2   elxmp://el-xmp/xmlns/:FilePath
;;     3   elxmp://el-xmp/xmlns/:FileParent
;;     4   elxmp://el-xmp/xmlns/:FileModifyTime
;;     5   elxmp://el-xmp/xmlns/:TargetProperties
;;     6   elxmp://el-xmp/xmlns/:PropertyIdList
;;     99  elxmp://el-xmp/xmlns/:EndOfReservedId
;;     100 http://ns.adobe.com/xap/1.0/:CreateDate
;;     101 http://ns.adobe.com/xap/1.0/:Rating
;;     102 http://ns.adobe.com/xap/1.0/:Label
;;     103 http://purl.org/dc/elements/1.1/:subject
;;     (See:`xmp-sqlite-odb-get-property-id', 'xmp-sqlite-odb-define-property')

;;   object_property_values
;;     object_id  property_id        value
;;     ----------+-------------------+--------
;;     1          1(ObjectType)       Directory
;;     1          2(FilePath)         C:/Users/user1/Pictures/
;;     2          1(ObjectType)       PropertyIdList
;;     2          6(PropertyIdList)   100 101 102 103
;;     3          1(ObjectType)       File
;;     3          2(FilePath)         C:/Users/user1/Pictures/flower.jpg
;;     3          3(FileParent)       1
;;     3          4(FileModifyTime)   1729499635.0
;;     3          5(TargetProperties) 2
;;     3          100(CreateDate)     2024-11-01T09:23:11.278+09:00
;;     3          101(Label)          Red
;;     3          103(subject)        \x01(:pv-type array :value ((:pv-type text :value "Flower") (:pv-type text :value "Photo")) :array-type (:http://www.w3.org/1999/02/22-rdf-syntax-ns\# . "Bag"))
;;
;;    - Create object
;;      - `xmp-sqlite-odb-new-object'
;;    - Set properties
;;      - `xmp-sqlite-odb-set-object-properties'
;;      - `xmp-sqlite-odb-set-object-property'
;;    - Get properties
;;      - `xmp-sqlite-odb-get-object-properties'
;;      - `xmp-sqlite-odb-get-object-property'
;;    - Reverse lookup
;;      - `xmp-sqlite-odb-get-objects-by-property-value'
;;      - `xmp-sqlite-odb-get-object-by-property-value'

(defconst xmp-sqlite-odb-version #x100)

(defconst xmp-sqlite-elxmp:ObjectType
  (xmp-xml-ename xmp-elxmp: "ObjectType"))
(defconst xmp-sqlite-elxmp:FilePath
  (xmp-xml-ename xmp-elxmp: "FilePath"))
(defconst xmp-sqlite-elxmp:FileParent
  (xmp-xml-ename xmp-elxmp: "FileParent"))
(defconst xmp-sqlite-elxmp:FileModifyTime
  (xmp-xml-ename xmp-elxmp: "FileModifyTime"))
(defconst xmp-sqlite-elxmp:TargetProperties
  (xmp-xml-ename xmp-elxmp: "TargetProperties"))
(defconst xmp-sqlite-elxmp:PropertyIdList
  (xmp-xml-ename xmp-elxmp: "PropertyIdList"))
(defconst xmp-sqlite-elxmp:EndOfReservedId
  (xmp-xml-ename xmp-elxmp: "EndOfReservedId"))

(defconst xmp-sqlite-odb-reserved-properties
  (list (cons 1 xmp-sqlite-elxmp:ObjectType)
        (cons 2 xmp-sqlite-elxmp:FilePath)
        (cons 3 xmp-sqlite-elxmp:FileParent)
        (cons 4 xmp-sqlite-elxmp:FileModifyTime)
        (cons 5 xmp-sqlite-elxmp:TargetProperties)
        (cons 6 xmp-sqlite-elxmp:PropertyIdList)
        (cons 99 xmp-sqlite-elxmp:EndOfReservedId)))

(cl-defstruct (xmp-sqlite-odb (:constructor xmp-sqlite-odb--new))
  db
  property-id-alist
  (info nil))

(defun xmp-sqlite-odb-close (odb)
  "Close ODB."
  (when (xmp-sqlite-odb-db odb)
    (sqlite-close (xmp-sqlite-odb-db odb))
    (setf (xmp-sqlite-odb-db odb) nil)
    t))

(defun xmp-sqlite-odb-open (db-file)
  "Open DB-FILE as an object database.
Return a `xmp-sqlite-odb' structure."
  (unless (file-exists-p db-file)
    (let ((dir (file-name-directory (expand-file-name db-file))))
      (unless (file-directory-p dir)
        (with-file-modes #o700
          (make-directory dir t)))))
  (let ((db (sqlite-open db-file)))
    (unless db (error "Failed to open DB file `%s'" db-file))
    (condition-case err
        (progn
          (let ((db-version (xmp-sqlite-odb-open--get-db-version db)))
            (pcase db-version
              ('nil
               (with-sqlite-transaction db
                 (xmp-sqlite-odb-open--create-tables db)
                 (xmp-sqlite-odb-open--insert-reserved-properties db)))
              ((pred integerp)
               (when (/= (/ db-version 256) (/ xmp-sqlite-odb-version 256))
                 (error "DB file `%s' is an incompatible version (#x%x)"
                        db-file db-version)))
              (_
               (error "DB file `%s' has invalid version `%s'"
                      db-file db-version))))
          (xmp-sqlite-odb--new
           :db db
           :property-id-alist (xmp-sqlite-odb-open--get-property-id-alist db)))
      (error
       (sqlite-close db)
       (signal (car err) (cdr err))))))
;; (xmp-sqlite-cache-odb)

(defun xmp-sqlite-odb-open--get-db-version (db)
  (when (eq (caar
             (sqlite-select
              db
              "select count(*) from sqlite_master
               where type='table' and name='elxmp_db_info'"))
            1)
    (caar (sqlite-select
           db "select value from elxmp_db_info where key='version'"))))

(defun xmp-sqlite-odb-open--create-tables (db)
  (sqlite-execute
   db
   "create table if not exists elxmp_db_info (key text primary key, value)")
  (sqlite-execute
   db
   "insert or ignore into elxmp_db_info values
    ('version', ?),
    ('last_object_id', 0)"
   (list xmp-sqlite-odb-version))

  (sqlite-execute
   db
   "create table if not exists properties
    (id integer primary key, name text not null unique, type integer);")

  (sqlite-execute
   db
   "create table if not exists object_property_values
    (object_id integer not null,
     property_id integer not null,
     value blob,
     primary key(object_id, property_id)) without rowid;")

  (sqlite-execute
   db
   "create index if not exists value_properties
    on object_property_values(property_id, value);"))

(defun xmp-sqlite-odb-open--insert-reserved-properties (db)
  (sqlite-execute
   db
   (concat "insert into properties (id,name) values "
           (xmp-sqlite-make-string-repeated
            "(?,?)"
            (length xmp-sqlite-odb-reserved-properties)
            ","))
   (cl-loop for (id . ename) in xmp-sqlite-odb-reserved-properties
            collect id
            collect (xmp-xml-ename-string ename nil nil t)))) ;; always-uri

(defun xmp-sqlite-odb-open--get-property-id-alist (db)
  (cl-loop for (id name)
           in (sqlite-select db "select id, name from properties")
           for parts = (split-string name ":") ;; NSNAME(URI):LOCAL
           for ns-name-str = (when (cdr parts) (pop parts))
           for local-name = (car parts)
           when (and ns-name-str local-name)
           collect (cons (xmp-xml-ename (xmp-xml-ns-name ns-name-str)
                                        local-name)
                         id)))
;; EXAMPLE: (xmp-sqlite-odb-open--get-property-id-alist (xmp-sqlite-odb-db (xmp-sqlite-cache-odb)))

(defun xmp-sqlite-odb-update-property-id-alist (odb)
  (when-let ((alist (xmp-sqlite-odb-open--get-property-id-alist
                     (xmp-sqlite-odb-db odb))))
    (setf (xmp-sqlite-odb-property-id-alist odb) alist)))
;; EXAMPLE: (xmp-sqlite-odb-update-property-id-alist odb)

(defun xmp-sqlite-odb-define-property (odb ename)
  "Register a property named ENAME in the ODB and return the property ID."
  (ignore-errors
    (let ((db (xmp-sqlite-odb-db odb))
          (name (xmp-xml-ename-string ename nil nil t))) ;; always-uri
      (sqlite-execute db
                      "insert or ignore into properties (name) values(?)"
                      (list name))
      (let ((id (caar (sqlite-select db
                                     "select id from properties where name=?"
                                     (list name)))))
        (setf (alist-get ename (xmp-sqlite-odb-property-id-alist odb)
                         nil nil #'xmp-xml-ename-equal)
              id)
        id))))
;; EXAMPLE: (xmp-sqlite-define-property (xmp-sqlite-cache-odb) xmp-xmp:Rating)

(defun xmp-sqlite-odb-get-property-id (odb ename)
  "Return the ID in the ODB of the property named ENAME.
If the property is not defined in the ODB, register it."
  (or (xmp-xml-ename-alist-get ename (xmp-sqlite-odb-property-id-alist odb))
      (xmp-sqlite-odb-define-property odb ename)
      (error "Unregistered property name `%s'" ename)))
;; EXAMPLE: (xmp-sqlite-odb-get-property-id (xmp-sqlite-cache-odb) xmp-sqlite-elxmp:ObjectType)
;; EXAMPLE: (xmp-sqlite-odb-get-property-id (xmp-sqlite-cache-odb) xmp-xmp:Rating)
;; EXAMPLE: (xmp-sqlite-odb-get-property-id (xmp-sqlite-cache-odb) xmp-xmp:Label)

(defun xmp-sqlite-odb-get-property-name (odb property-id)
  "Return the property name in the ODB that corresponds to the PROPERTY-ID."
  (or (car (rassq property-id
                  (xmp-sqlite-odb-property-id-alist odb)))
      ;; TODO: Error?
      (car property-id)))

(defun xmp-sqlite-odb-new-object-id (odb)
  "Create a new object ID."
  (caar
   (sqlite-execute
    (xmp-sqlite-odb-db odb)
    "update elxmp_db_info
     set value=(select value from elxmp_db_info where key='last_object_id')+1
     where key='last_object_id' returning value")))
;; EXAMPLE: (xmp-sqlite-odb-new-object-id (xmp-sqlite-cache-odb))

(defun xmp-sqlite-odb-set-object-property (odb object-id prop-ename value)
  "Set a property on an object in the ODB.
OBJECT-ID is the ID of the object.
PROP-ENAME is the property name.
 VALUE is the property value."
  (sqlite-execute
   (xmp-sqlite-odb-db odb)
   "insert or replace into object_property_values (object_id,property_id,value)
    values (?,?,?)"
   (list object-id (xmp-sqlite-odb-get-property-id odb prop-ename) value)))
;; EXAMPLE: (let* ((odb (xmp-sqlite-cache-odb)) (object-id (xmp-sqlite-odb-new-object-id odb))) (xmp-sqlite-odb-set-object-property odb object-id xmp-sqlite-elxmp:ObjectType "File") (xmp-sqlite-odb-set-object-property odb object-id xmp-xmp:Label "Red"))

(defun xmp-sqlite-make-string-repeated (string count separator)
  (cond
   ((<= count 0) "")
   ((= count 1) string)
   (t ;; (>= count 1)
    (let ((result string)
          (sep-str (concat separator string)))
      (setq count (1- count))
      (while (> count 0)
        (setq result (concat result sep-str)
              count (1- count)))
      result))))
;; TEST: (xmp-sqlite-make-string-repeated "?" 3 ",") => "?,?,?"

(defun xmp-sqlite-odb-set-object-properties (odb object-id prop-ename-alist)
  "Set multiple properties on an object in the ODB.
OBJECT-ID is the ID of the object.
PROP-ENAME-ALIST is an association list of property names and property values."
  (when prop-ename-alist
    ;; Setting properties collectively was faster than setting them
    ;; individually. (Time to update 4 properties x 1000 times:
    ;; Individual: 22.3s, Collective: 6.1s
    ;; (cl-loop for (ename . value) in prop-ename-alist
    ;;          do (xmp-sqlite-set-object-property object-id ename value))
    (sqlite-execute
     (xmp-sqlite-odb-db odb)
     (concat
      "insert or replace into object_property_values "
      "(object_id,property_id,value) values "
      (xmp-sqlite-make-string-repeated "(?,?,?)" (length prop-ename-alist) ","))
     (cl-loop for (prop-ename . value) in prop-ename-alist
              collect object-id
              collect (xmp-sqlite-odb-get-property-id odb prop-ename)
              collect value))))
;; EXAMPLE: (let* ((odb (xmp-sqlite-cache-odb)) (object-id (xmp-sqlite-odb-new-object_id odb))) (xmp-sqlite-odb-set-object-properties odb object-id (list (cons xmp-sqlite-elxmp:ObjectType "File") (cons xmp-sqlite-elxmp:FilePath "C:/home/example.jpg") (cons xmp-xmp:Label "Red") (cons xmp-xmp:Rating 5))))
;; EXAMPLE: (let* ((odb (xmp-sqlite-cache-odb)) (object-id (xmp-sqlite-odb-new-object_id odb))) (benchmark-run 1000 (xmp-sqlite-odb-set-object-properties odb object-id (list (cons xmp-sqlite-elxmp:ObjectType "File") (cons xmp-sqlite-elxmp:FilePath "C:/home/example.jpg") (cons xmp-xmp:Label "Red") (cons xmp-xmp:Rating 5)))))

(defun xmp-sqlite-odb-new-object (odb prop-ename-alist)
  "Create a new object in the ODB.
PROP-ENAME-ALIST is an association list of property names and property values.
Return the object ID."
  (let ((object-id (xmp-sqlite-odb-new-object-id odb)))
    (xmp-sqlite-odb-set-object-properties odb object-id prop-ename-alist)
    object-id))

(defun xmp-sqlite-odb-get-object-properties (odb object-id)
  "Return a list of the properties of the object specified by OBJECT-ID."
  (let ((result
         (sqlite-select
          (xmp-sqlite-odb-db odb)
          "select property_id,value from object_property_values
           where object_id=?"
          (list object-id))))
    (dolist (pv result)
      ;; (PROPERTY-ID VALUE) => (PROP-ENAME . VALUE)
      (let ((property-id (car pv))
            (value (cadr pv)))
        (setcar pv (xmp-sqlite-odb-get-property-name odb property-id))
        (setcdr pv value)))
    result))
;; EXAMPLE: (xmp-sqlite-odb-get-object-properties (xmp-sqlite-cache-odb) 8)

(defun xmp-sqlite-odb-get-object-property (odb object-id prop-ename)
  "Return the value of the property specified by PROP-NAME of the object
specified by OBJECT-ID."
  (caar
   (sqlite-select
    (xmp-sqlite-odb-db odb)
    "select value from object_property_values
           where object_id=? and property_id=?"
    (list object-id (xmp-sqlite-odb-get-property-id odb prop-ename)))))
;; EXAMPLE: (xmp-sqlite-odb-get-object-property (xmp-sqlite-cache-odb) 8 xmp-sqlite-elxmp:ObjectType)

(defun xmp-sqlite-odb-get-objects-by-property-value (odb prop-ename value)
  "Return a list of object IDs whose property matches the value."
  (mapcan
   #'identity ;; unwrap ((x) (y) (z)) => (x y z)
   (sqlite-select
    (xmp-sqlite-odb-db odb)
    "select object_id from object_property_values
     where property_id=? and value=?"
    (list (xmp-sqlite-odb-get-property-id odb prop-ename) value))))
;; EXAMPLE: (xmp-sqlite-odb-get-objects-by-property-value (xmp-sqlite-cache-odb) xmp-xmp:Label "Red")

(defun xmp-sqlite-odb-get-object-by-property-value (odb prop-ename value)
  "Return only one ID of the object whose property matches the value.
If there are multiple matches, it is not clear which object's ID will be
returned."
  (caar
   (sqlite-select
    (xmp-sqlite-odb-db odb)
    "select object_id from object_property_values
     where property_id=? and value=? limit 1"
    (list (xmp-sqlite-odb-get-property-id odb prop-ename) value))))
;; EXAMPLE: (xmp-sqlite-odb-get-object-by-property-value (xmp-sqlite-cache-odb) xmp-xmp:Label "Red")

(defun xmp-sqlite-odb-delete-object (odb object-id)
  "Delete the object specified by OBJECT-ID."
  (sqlite-execute
   (xmp-sqlite-odb-db odb)
   "delete from object_property_values where object_id = ?"
   (list object-id)))
;; EXAMPLE: (xmp-sqlite-odb-delete-object (xmp-sqlite-cache-odb) 8)


;;;; PropertyIdList

;; PropertyIdList is a database object that simply holds multiple property IDs.

;; In the object_property_values table:
;;   <object-id> elxmp:ObjectType "PropertyIdList"
;;   <object-id> elxmp:PropertyIdList "<property-id-decimal> ..."

(defun xmp-sqlite-property-id-list-make-string (odb prop-ename-list)
  "Convert PROP-ENAME-LIST into a string for storage in a PropertyIdList
object."
  (mapconcat (lambda (prop-ename)
               (number-to-string
                (xmp-sqlite-odb-get-property-id odb prop-ename)))
             prop-ename-list
             " "))

(defun xmp-sqlite-property-id-list-parse-string (odb string)
  "Convert the STRING stored in the PropertyIdList object to a PROP-ENAME-LIST."
  (cl-loop for num-str in (split-string string)
           for property-id = (string-to-number num-str)
           collect (xmp-sqlite-odb-get-property-name odb property-id)))

(defmacro xmp-sqlite-property-id-list-objects (odb)
  ;; ((PROP-ENAME-LIST . OBJECT-ID)...)
  `(alist-get :property-id-list-objects (xmp-sqlite-odb-info ,odb)))

(defun xmp-sqlite-property-id-list-get-object-id (odb prop-ename-list)
  "Return the ID of the PropertyIdList object that holds the PROP-ENAME-LIST."
  (or
   ;; Search from cache by object (Fast)
   (cdr (assq prop-ename-list (xmp-sqlite-property-id-list-objects odb)))
   ;; Search from cache by value (Slow)
   ;; TODO: Swap the car of the cell and PROP-ENAME-LIST?
   (cdr (assoc prop-ename-list (xmp-sqlite-property-id-list-objects odb)
               #'xmp-xml-ename-list-equal))
   ;; Search from database or create
   (let ((value-str (xmp-sqlite-property-id-list-make-string odb
                                                             prop-ename-list)))
     (when-let ((object-id
                 (or
                  ;; From an existing object
                  (xmp-sqlite-odb-get-object-by-property-value
                   odb xmp-sqlite-elxmp:PropertyIdList value-str)
                  ;; New object
                  (xmp-sqlite-odb-new-object
                   odb
                   (list
                    (cons xmp-sqlite-elxmp:ObjectType "PropertyIdList")
                    (cons xmp-sqlite-elxmp:PropertyIdList value-str))))))
       ;; Cache
       (push (cons prop-ename-list object-id)
             (xmp-sqlite-property-id-list-objects odb))
       object-id))))

(defun xmp-sqlite-property-id-list-from-object-id (odb object-id)
  "Get PROP-ENAME-LIST from the PropertyIdList object pointed to by OBJECT-ID."
  (when (integerp object-id)
    (or
     ;; From cache
     (car (rassq object-id (xmp-sqlite-property-id-list-objects odb)))
     ;; From database
     (when-let ((value-str (xmp-sqlite-odb-get-object-property
                            odb object-id xmp-sqlite-elxmp:PropertyIdList)))
       (let ((prop-ename-list
              (xmp-sqlite-property-id-list-parse-string odb value-str)))
         ;; Cache
         (push (cons prop-ename-list object-id)
               (xmp-sqlite-property-id-list-objects odb))
         prop-ename-list)))))


;;;; Convert between PValue and DBValue

(defconst xmp-sqlite-vtype-sexp 1)
(defconst xmp-sqlite-vtype-text 2)
(defconst xmp-sqlite-vtype-max 8)

(defun xmp-sqlite-encode-pvalue (pvalue)
  (cond
   ((null pvalue) nil)
   ((xmp-pvalue-text-p pvalue)
    (xmp-sqlite-encode-pvalue-as-text pvalue))
   ;;(xmp-pvalue-qualifier-alist pvalue)
   ;;((xmp-pvalue-uri-p pvalue)
   ;;((xmp-pvalue-array-p pvalue)
   ;;((xmp-pvalue-struct-p pvalue)
   (t
    (xmp-sqlite-encode-pvalue-as-sexp pvalue))))

(defun xmp-sqlite-encode-pvalue-as-sexp (pvalue)
  (concat (make-string 1 xmp-sqlite-vtype-sexp) (prin1-to-string pvalue)))

(defun xmp-sqlite-encode-pvalue-as-text (pvalue)
  (let ((text (xmp-pvalue-as-text pvalue)))
    (if (and (>= (length text) 1)
             (<= (aref text 0) xmp-sqlite-vtype-max))
        (concat (make-string 1 xmp-sqlite-vtype-text)
                text)
      text)))

(defun xmp-sqlite-decode-pvalue (db-value)
  (cond
   ((null db-value) nil)
   ((stringp db-value)
    (if (and (>= (length db-value) 1)
             (<= (aref db-value 0) xmp-sqlite-vtype-max))
        (let ((vtype (aref db-value 0)))
          (cond
           ((= vtype xmp-sqlite-vtype-text)
            (xmp-pvalue-make-text (substring db-value 1)))
           ((= vtype xmp-sqlite-vtype-sexp)
            (read (substring db-value 1)))
           (t
            nil)))
      (xmp-pvalue-make-text db-value)))))



;;;; File XMP Metadata Cache

;; These are called by the following functions:
;; - `xmp-file-cache-get-properties'
;; - `xmp-file-cache-make-entry'

;; Automatic closing
;; Close databases that have not been used for an extended period of time.

(defcustom xmp-sqlite-db-auto-close t
  "Non-nil means to automatically close the database."
  :group 'xmp :type 'boolean)

(defvar xmp-sqlite-db-auto-close-timer nil)
(defvar xmp-sqlite-db-auto-close-last-access nil)

(defconst xmp-sqlite-db-auto-close-test-period (* 60 5))
(defconst xmp-sqlite-db-auto-close-timeout (* 60 10))

(defun xmp-sqlite-db-auto-close-set-timer ()
  (unless xmp-sqlite-db-auto-close-timer
    (setq xmp-sqlite-db-auto-close-timer
          (run-with-timer xmp-sqlite-db-auto-close-test-period
                          nil #'xmp-sqlite-db-auto-close-on-timer))))

(defun xmp-sqlite-db-auto-close-cancel-timer ()
  (when xmp-sqlite-db-auto-close-timer
    (cancel-timer xmp-sqlite-db-auto-close-timer)
    (setq xmp-sqlite-db-auto-close-timer nil)))

(defun xmp-sqlite-db-auto-close-on-timer ()
  (setq xmp-sqlite-db-auto-close-timer nil)
  (if (> (- (float-time) xmp-sqlite-db-auto-close-last-access)
         xmp-sqlite-db-auto-close-timeout)
      (progn
        (message "Close XMP cache DB")
        (xmp-sqlite-cache-odb-close))
    (xmp-sqlite-db-auto-close-set-timer)))

(defun xmp-sqlite-db-auto-close-touch ()
  (when xmp-sqlite-db-auto-close
    (setq xmp-sqlite-db-auto-close-last-access (float-time))
    (xmp-sqlite-db-auto-close-set-timer)))

(defun xmp-sqlite-db-auto-close-on-close-db ()
  (xmp-sqlite-db-auto-close-cancel-timer))

;; Cache DB - Open / Close

(defcustom xmp-sqlite-cache-db-file
  (expand-file-name "el-xmp/el-xmp-file-cache.db" user-emacs-directory)
  "Location of the metadata cache database."
  :group 'xmp :type 'file)

(defvar xmp-sqlite-cache-odb nil)

(defun xmp-sqlite-cache-odb ()
  (xmp-sqlite-db-auto-close-touch)
  (if (and xmp-sqlite-cache-odb
           (xmp-sqlite-odb-db xmp-sqlite-cache-odb))
      xmp-sqlite-cache-odb
    (setq xmp-sqlite-cache-odb (xmp-sqlite-odb-open xmp-sqlite-cache-db-file))))
;; EXAMPLE: (xmp-sqlite-cache-odb)

(defun xmp-sqlite-cache-odb-close ()
  (interactive)
  (when xmp-sqlite-cache-odb
    (prog1 (xmp-sqlite-odb-close xmp-sqlite-cache-odb)
      (setq xmp-sqlite-cache-odb nil)
      (xmp-sqlite-db-auto-close-on-close-db))))
;; EXAMPLE: (xmp-sqlite-cache-odb-close)

;; Cache DB - Access file entries

(defun xmp-sqlite-cache-get-file-properties (file)
  (when-let ((object-id (xmp-sqlite-odb-get-object-by-property-value
                         (xmp-sqlite-cache-odb)
                         xmp-sqlite-elxmp:FilePath
                         (expand-file-name file))))
    (cons
     object-id
     (xmp-sqlite-odb-get-object-properties (xmp-sqlite-cache-odb)
                                           object-id))))

(defun xmp-sqlite-cache-get-dir-entry-or-create (dir)
  ;; Ensure that DIR is `/' terminated and an absolute path.
  (setq dir (file-name-as-directory (expand-file-name dir)))

  (if-let ((object-id (xmp-sqlite-odb-get-object-by-property-value
                       (xmp-sqlite-cache-odb)
                       xmp-sqlite-elxmp:FilePath
                       dir)))
      object-id
    (xmp-sqlite-odb-new-object
     (xmp-sqlite-cache-odb)
     (list (cons xmp-sqlite-elxmp:ObjectType "Directory")
           (cons xmp-sqlite-elxmp:FilePath dir)))))

(defun xmp-sqlite-cache-make-file-entry (file file-attrs properties
                                              target-prop-ename-list)
  (setq file (expand-file-name file))
  (let* ((object-id (xmp-sqlite-odb-get-object-by-property-value
                     (xmp-sqlite-cache-odb)
                     xmp-sqlite-elxmp:FilePath
                     file))
         (db-properties
          (nconc
           (list (cons xmp-sqlite-elxmp:ObjectType "File")
                 (cons xmp-sqlite-elxmp:FilePath file)
                 (cons xmp-sqlite-elxmp:FileParent
                       (xmp-sqlite-cache-get-dir-entry-or-create
                        (file-name-directory file)))
                 (cons xmp-sqlite-elxmp:FileModifyTime
                       (float-time
                        (file-attribute-modification-time file-attrs)))
                 (cons xmp-sqlite-elxmp:TargetProperties
                       (xmp-sqlite-property-id-list-get-object-id
                        (xmp-sqlite-cache-odb)
                        target-prop-ename-list))
                 )
           ;; Append XMP properties
           (cl-loop for (ename . pvalue) in properties
                    collect (cons ename
                                  (xmp-sqlite-encode-pvalue pvalue))))))
    (if object-id
        (progn
          (xmp-sqlite-odb-delete-object (xmp-sqlite-cache-odb) object-id)
          (xmp-sqlite-odb-set-object-properties (xmp-sqlite-cache-odb)
                                                object-id
                                                db-properties))
      (xmp-sqlite-odb-new-object
       (xmp-sqlite-cache-odb)
       db-properties))))

(defun xmp-sqlite-cache-get-file-entry (file)
  (setq file (expand-file-name file))
  (let* ((odb (xmp-sqlite-cache-odb))
         (object-id (xmp-sqlite-odb-get-object-by-property-value
                     odb xmp-sqlite-elxmp:FilePath file))
         (db-properties (when object-id
                          (xmp-sqlite-odb-get-object-properties odb
                                                                object-id))))
    (when object-id
      (let* ((modtime
              (xmp-xml-ename-alist-get xmp-sqlite-elxmp:FileModifyTime
                                       db-properties))
             (target-prop-ename-list
              (xmp-sqlite-property-id-list-from-object-id
               odb
               (xmp-xml-ename-alist-get xmp-sqlite-elxmp:TargetProperties
                                        db-properties)))
             (properties
              (cl-loop for (ename . db-value) in db-properties
                       when (xmp-xml-ename-member ename target-prop-ename-list)
                       collect (cons ename
                                     (xmp-sqlite-decode-pvalue db-value)))))
        (xmp-file-cache-file-entry-make (file-name-nondirectory file)
                                        modtime
                                        properties
                                        target-prop-ename-list)))))

(defun xmp-sqlite-cache-remove-file-entry (file)
  (setq file (expand-file-name file))
  (let* ((odb (xmp-sqlite-cache-odb))
         (object-id (xmp-sqlite-odb-get-object-by-property-value
                     odb xmp-sqlite-elxmp:FilePath file)))
    (when object-id
      (xmp-sqlite-odb-delete-object odb object-id))))

(provide 'xmp-sqlite)
;;; xmp-sqlite.el ends here
