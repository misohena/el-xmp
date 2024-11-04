;;; xmp-tiff.el --- TIFF format library             -*- lexical-binding: t; -*-

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

;; The following example shows how to get the image size of example.tif.
;;
;; Example1:
;; (with-temp-buffer
;;   (let* ((reader (xmp-file-reader-open "example.tif"))
;;          (tiff-header (xmp-tiff-read-header reader))
;;          (0th-fields (xmp-tiff-read-0th-ifd reader tiff-header 'all))
;;          (field-image-width (xmp-tiff-field-get 256 0th-fields))
;;          (field-image-length (xmp-tiff-field-get 257 0th-fields)))
;;     (list
;;      :width
;;      (xmp-tiff-field-value-uint field-image-width reader tiff-header)
;;      :height
;;      (xmp-tiff-field-value-uint field-image-length reader tiff-header))))

;; The following example lists the names and values of all fields
;; recorded in the EXIF IFD of example.tif.
;;
;; Example2:
;; (with-temp-buffer
;;   (let* ((reader (xmp-file-reader-open "example.tif"))
;;          (tiff-header (xmp-tiff-read-header reader))
;;          (0th-fields (xmp-tiff-read-0th-ifd reader tiff-header
;;                                             (list xmp-tiff-tag-exif-ifd)))
;;          (exif-ifd-field (xmp-tiff-field-get xmp-tiff-tag-exif-ifd
;;                                              0th-fields))
;;          (exif-fields (xmp-tiff-read-pointed-ifd reader tiff-header
;;                                                  exif-ifd-field 'all)))
;;     (mapcar (lambda (field)
;;               (cons
;;                (xmp-tiff-tag-name (xmp-tiff-field-tag field)
;;                                   xmp-exif-exif-tags)
;;                (xmp-tiff-field-values field reader tiff-header)))
;;             exif-fields)))

;;; References:

;; [EXIF3]
;; CIPA DC-008-2023,
;; Exchangeable image file format for digital still cameras: Exif Version 3.0,
;; https://www.cipa.jp/std/documents/download_j.html?DC-008-2023-J

;; [EXIFXMP2024]
;; CIPA DC-010-2024, Exif metadata for XMP,
;; https://www.cipa.jp/std/documents/download_j.html?CIPA_DC-010-2024_E

;; [TIFF6]
;; TIFF Revision 6.0 Final, Adobe Systems Incorporated, June 1992,
;; https://pdfa.org/norm-refs/TIFF6.pdf

;; [TIFF-F]
;; RFC 2306, Tag Image File Format (TIFF) - F Profile for Facsimile, March 1998.

;; [TIFF-FX]
;; RF 2301, File Format for Internet Fax, March 1998

;;; Code:

(require 'cl-lib)
(require 'xmp-file-reader)

;;;; TIFF Header

(cl-defstruct (xmp-tiff-header (:constructor xmp-tiff-header))
  "TIFF header information read by `xmp-tiff-read-header'."
  (tiff-offset
   nil :documentation "Start position of the TIFF header in `xmp-file-reader'.")
  (be
   nil :documentation "Big endian flag.")
  (0th-ifd-offset
   nil :documentation
   "Start position of 0th IFD from beginning of the TIFF header."))

(defun xmp-tiff-read-header (reader)
  "Read a TIFF header and return an `xmp-tiff-header' structure."
  (let* ((tiff-offset (xmp-file-reader-current-offset reader))
         (be (pcase (xmp-file-reader-u16be reader)
               (#x4d4d t) ;;MM
               (#x4949 nil) ;;II
               (marker (error "Invalid endian `%04X' in tiff header" marker))))
         (magic (xmp-file-reader-u16 reader be))
         (0th-ifd-offset (xmp-file-reader-u32 reader be)))
    (unless (= magic 42)
      (error "Invalid magic number `%04X' in tiff header" magic))
    (xmp-tiff-header :tiff-offset tiff-offset
                     :be be
                     :0th-ifd-offset 0th-ifd-offset)))

;;;; TIFF Field

(cl-defstruct (xmp-tiff-field
               (:constructor xmp-tiff-field)
               ;; NOTE: (car field) must be a tag number for assq
               (:type list))
  "TIFF field entry read by `xmp-tiff-read-ifd'."
  tag type value-count value-offset field-offset)

(defun xmp-tiff-read-0th-ifd (reader tiff-header tags-to-read)
  (xmp-tiff-seek-and-read-ifd reader tiff-header
                              (xmp-tiff-header-0th-ifd-offset tiff-header)
                              tags-to-read))

(defun xmp-tiff-read-pointed-ifd (reader tiff-header
                                         pointer-field
                                         tags-to-read)
  (let ((offset (xmp-tiff-field-value-single pointer-field reader tiff-header)))
    (xmp-tiff-seek-and-read-ifd reader tiff-header offset tags-to-read)))

(defun xmp-tiff-seek-and-read-ifd (reader tiff-header
                                          ifd-offset-from-tiff-header
                                          tags-to-read)
  (xmp-file-reader-seek reader (+ (xmp-tiff-header-tiff-offset tiff-header)
                                  ifd-offset-from-tiff-header))
  (xmp-tiff-read-ifd reader tiff-header tags-to-read))

(defun xmp-tiff-read-ifd (reader tiff-header tags-to-read)
  "Read field entries from the READER's current position.
Return a list of `xmp-tiff-field' structures.

TAGS-TO-READ is a list of tag numbers of fields to read. If this is nil,
nothing is read. If this is a non-nil symbol (`all' is recommended), all
fields are read."
  (when (consp tags-to-read)
    (setq tags-to-read (copy-sequence tags-to-read)))
  (cl-loop with be = (xmp-tiff-header-be tiff-header)
           repeat (xmp-file-reader-u16 reader be) ;; number of fields
           while tags-to-read
           for field-offset = (- (xmp-file-reader-current-offset reader)
                                 (xmp-tiff-header-tiff-offset tiff-header))
           for tag = (xmp-file-reader-u16 reader be)
           if (or (not (consp tags-to-read)) ;; tags-to-read=t
                  (memq tag tags-to-read))
           do (when (consp tags-to-read)
                (setq tags-to-read (delq tag tags-to-read)))
           and collect (xmp-tiff-field
                        :tag tag
                        :type (xmp-file-reader-u16 reader be)
                        :value-count (xmp-file-reader-u32 reader be)
                        :value-offset (xmp-file-reader-u32 reader be)
                        :field-offset field-offset)
           else
           do (xmp-file-reader-skip reader 10))
  ;; (xmp-file-reader-skip reader (* 12 num-fields))
  ;; (xmp-file-reader-u32 reader be) ;; Next IFD
  )

(defun xmp-tiff-field-get (tag fields &optional noerror)
  "Find the field with TAG in the list of FIELDS read by `xmp-tiff-read-ifd'.

If not found, signal an error. However, if NOERROR is non-nil, return nil."
  (or (assq tag fields)
      (if noerror
          nil
        (error "No field with tag %d." tag))))

(defun xmp-tiff-field-values-size (field)
  "Return the total number of bytes in the FIELD's values."
  (let ((count (xmp-tiff-field-value-count field)))
    (pcase (xmp-tiff-field-type field)
      (1 (* 1 count)) ;; BYTE
      (2 count) ;; ASCII
      (3 (* 2 count)) ;; SHORT
      (4 (* 4 count)) ;; LONG
      (5 (* 8 count)) ;; RATIONAL
      (6 (* 1 count)) ;; SBYTE
      (7 count) ;; UNDEFINED
      (8 (* 2 count)) ;; SSHORT
      (9 (* 4 count)) ;; SLONG
      (10 (* 8 count)) ;; SRATIONAL
      (11 (* 4 count)) ;; FLOAT
      (12 (* 8 count)) ;; DOUBLE
      (129 count) ;; UTF-8 ([EXIF3] 4.6.2)
      )))

(defun xmp-tiff-read-values (reader be field)
  "Read the values of the FIELD from the READER."
  (let ((count (xmp-tiff-field-value-count field)))
    (when (> count 0)
      (pcase (xmp-tiff-field-type field)
        (1 (cl-loop repeat count
                    collect (xmp-file-reader-u8 reader))) ;; BYTE
        (2 (let ((bytes (xmp-file-reader-read-bytes reader count)))
             (mapcar (lambda (s) (decode-coding-string s 'utf-8))
                     (split-string (substring bytes 0 -1) "\0")))) ;; ASCII
        (3 (cl-loop repeat count
                    collect (xmp-file-reader-u16 reader be))) ;; SHORT
        (4 (cl-loop repeat count
                    collect (xmp-file-reader-u32 reader be))) ;; LONG
        (5 (cl-loop repeat count
                    collect (cons
                             (xmp-file-reader-u32 reader be)
                             (xmp-file-reader-u32 reader be)))) ;; RATIONAL
        (6 (cl-loop repeat count
                    collect (xmp-file-reader-s8 reader))) ;; SBYTE
        (7 nil) ;; UNDEFINED
        (8 (cl-loop repeat count
                    collect (xmp-file-reader-s16 reader be))) ;; SSHORT
        (9 (cl-loop repeat count
                    collect (xmp-file-reader-s32 reader be))) ;; SLONG
        (10 (cl-loop repeat count
                     collect (cons
                              (xmp-file-reader-s32 reader be)
                              (xmp-file-reader-s32 reader be)))) ;; SRATIONAL
        ;;(11 (xmp-file-reader-f32 reader be)) ;; FLOAT
        ;;(12 (xmp-file-reader-f64 reader be)) ;; DOUBLE
        (129 (let ((bytes (xmp-file-reader-read-bytes reader count)))
               (mapcar (lambda (s) (decode-coding-string s 'utf-8))
                       (split-string (substring bytes 0 -1) "\0")))) ;; UTF-8
        ))))

(defun xmp-tiff-field-value-bytes-range (field tiff-header)
  "Return the range in which the FIELD's values are located.

The range is represented by a cons cell whose car is the starting offset
and whose cdr is the size. The starting offset is a value that can be
passed directly to `xmp-file-reader-seek' (i.e. it includes the starting
offset of the TIFF header). The size is in bytes."
  (unless field (error "The field argument is nil."))

  (when-let ((field-size (xmp-tiff-field-values-size field)))
    (cons
     ;; Offset from beginning of file
     (if (<= field-size 4)
         ;; Read from the FIELD entry
         (+ (xmp-tiff-header-tiff-offset tiff-header)
            (xmp-tiff-field-field-offset field)
            ;; tag(2) + type(2) + count(4) : offset(4)
            8)
       ;; Read from the offset pointed to by the FIELD entry
       (+ (xmp-tiff-header-tiff-offset tiff-header)
          (xmp-tiff-field-value-offset field)))
     field-size)))

(defun xmp-tiff-field-values (field reader tiff-header)
  "Return a list of the values the field has."
  ;;(message "xmp-tiff-field-values field=%s" field)
  (unless field (error "The field argument is nil."))
  (when-let ((range (xmp-tiff-field-value-bytes-range field tiff-header)))
    (xmp-file-reader-seek reader (car range))
    (xmp-tiff-read-values reader (xmp-tiff-header-be tiff-header) field)))

(defun xmp-tiff-field-value-single (field reader tiff-header)
  "Return the only value the field has, or nil if the field has multiple
values."
  ;; TODO: Optimize (Check count)
  (let ((values (xmp-tiff-field-values field reader tiff-header)))
    (when (and values (null (cdr values)))
      (car values))))

(defun xmp-tiff-field-value-uint (field reader tiff-header)
  (let ((value (xmp-tiff-field-value-single field reader tiff-header)))
    (when (and (integerp value) (>= value 0))
      value)))

(defun xmp-tiff-field-value-sint (field reader tiff-header)
  (let ((value (xmp-tiff-field-value-single field reader tiff-header)))
    (when (integerp value)
      value)))

(defun xmp-tiff-field-value-string (field reader tiff-header)
  (let ((value (xmp-tiff-field-value-single field reader tiff-header)))
    (when (stringp value)
      value)))

;;;; Tag Information

(defconst xmp-tiff-tag-xmp-packet 700) ;; [XMP3] 1.1.6 TIFF
(defconst xmp-tiff-tag-exif-ifd 34665) ;; [EXIF3] 4.6.3.1.1
(defconst xmp-tiff-tag-gps-info-ifd 34853) ;; [EXIF3] 4.6.3.2.1

(defconst xmp-tiff-0th-tags
  ;; Tag TagName XMPNSPrefix XMPPropName XMPPropType XMPConvInfo
  ;; [EXIFXMP2024] 6 Mapping for TIFF metadata
  '((254 "NewSubfileType") ;; [TIFF6] 8: Baseline Field Reference Guide
    (255 "SubfileType") ;; [TIFF6] 8: Baseline Field Reference Guide
    (256 "ImageWidth" tiff "ImageWidth" Integer)
    (257 "ImageLength" tiff "ImageLength" Integer)
    (258 "BitsPerSample" tiff "BitsPerSample" SeqInteger)
    (259 "Compression" tiff "Compression" Integer)
    ;;260,261
    (262 "PhotometricInterpretation" tiff "PhotometricInterpretation" Integer)
    (263 "Threshholding") ;; [TIFF6] 8: Baseline Field Reference Guide
    (264 "CellWidth") ;; [TIFF6] 8: Baseline Field Reference Guide
    (265 "CellLength") ;; [TIFF6] 8: Baseline Field Reference Guide
    (266 "FillOrder") ;; [TIFF6] 8: Baseline Field Reference Guide
    ;;267,268
    (269 "DocumentName") ;; [TIFF6] 12: Document Storage and Retrieval
    (270 "ImageDescription" dc "description" LangAlt)
    (271 "Make" tiff "Make" Text)
    (272 "Model" tiff "Model" Text)
    (273 "StripOffsets") ;; No mapping given
    (274 "Orientation" tiff "Orientation" Integer)
    ;;275,276
    (277 "SamplesPerPixel" tiff "SamplesPerPixel" Integer)
    (278 "RowsPerStrip") ;; No mapping given
    (279 "StripByteCounts") ;; No mapping given
    (280 "MinSampleValue") ;; [TIFF6] 8: Baseline Field Reference Guide
    (281 "MaxSampleValue") ;; [TIFF6] 8: Baseline Field Reference Guide
    (282 "XResolution" tiff "XResolution" Rational)
    (283 "YResolution" tiff "YResolution" Rational)
    (284 "PlanarConfiguration" tiff "PlanarConfiguration" Integer)
    (285 "PageName") ;; [TIFF6] 12: Document Storage and Retrieval
    (286 "XPosition") ;; [TIFF6] 12: Document Storage and Retrieval
    (287 "YPosition") ;; [TIFF6] 12: Document Storage and Retrieval
    (288 "FreeOffsets") ;; [TIFF6] 8: Baseline Field Reference Guide
    (289 "FreeByteCounts") ;; [TIFF6] 8: Baseline Field Reference Guide
    (290 "GrayResponseUnit") ;; [TIFF6] 8: Baseline Field Reference Guide
    (291 "GrayResponseCurve") ;; [TIFF6] 8: Baseline Field Reference Guide
    (292 "T4Options") ;; [TIFF6] 11: CCITT Bilevel Encodings
    (293 "T6Options") ;; [TIFF6] 11: CCITT Bilevel Encodings
    ;;294,295
    (296 "ResolutionUnit" tiff "ResolutionUnit" Integer)
    (297 "PageNumber") ;; [TIFF6] 12: Document Storage and Retrieval
    ;;298,299,300
    (301 "TransferFunction" tiff "TransferFunction" SeqInteger)
    ;;302,303,304
    (305 "Software" xmp "CreatorTool" AgentName)
    (306 "DateTime" xmp "ModifyDate" Date (DSO #x9290 #x9010))
    ;;307~314
    (315 "Artist" dc "creator" SeqText)
    (316 "HostComputer") ;; [TIFF6] 8: Baseline Field Reference Guide
    (317 "Predictor") ;; [TIFF6] 14: Differencing Predictor
    (318 "WhitePoint" tiff "WhitePoint" SeqRational)
    (319 "PrimaryChromaticities" tiff "PrimaryChromaticities" SeqRational)
    (320 "ColorMap") ;; [TIFF6] 8: Baseline Field Reference Guide
    (321 "HalftoneHints") ;; [TIFF6] 17: HalftoneHints
    (322 "TileWidth") ;; [TIFF6] 15: Tiled Images
    (323 "TileLength") ;; [TIFF6] 15: Tiled Images
    (324 "TileOffsets") ;; [TIFF6] 15: Tiled Images
    (325 "TileByteCounts") ;; [TIFF6] 15: Tiled Images
    (326 "BadFaxLines") ;; [TIFF-F]
    (327 "CleanFaxData") ;; [TIFF-F]
    (328 "ConsecutiveBadFaxLines") ;; [TIFF-F]
    ;;329
    (330 "SubIFDs") ;; ???
    ;;331
    (332 "InkSet") ;; [TIFF6] 16: CMYK Images
    (333 "InkNames") ;; [TIFF6] 16: CMYK Images
    (334 "NumberOfInks") ;; [TIFF6] 16: CMYK Images
    ;;335
    (336 "DotRange") ;; [TIFF6] 16: CMYK Images
    (337 "TargetPrinter") ;; [TIFF6] 16: CMYK Images
    (338 "ExtraSamples") ;; [TIFF6] 8: Baseline Field Reference Guide
    (339 "SampleFormat") ;; [TIFF6] 19: Data Sample Format
    (340 "SMinSampleValue") ;; [TIFF6] 19: Data Sample Format
    (341 "SMaxSampleValue") ;; [TIFF6] 19: Data Sample Format
    (342 "TransferRange") ;; [TIFF6] 20: RGB Image Colorimetry
    ;;343~399
    (400 "GlobalParametersIFD") ;; [TIFF-FX] 2.2.4
    (401 "ProfileType") ;; [TIFF-FX] 2.2.4
    (402 "FaxProfile") ;; [TIFF-FX] 2.2.4
    (403 "CodingMethods") ;; [TIFF-FX] 2.2.4
    (404 "VersionYear") ;; [TIFF-FX] 2.2.4
    (405 "ModeNumber") ;; [TIFF-FX] 2.2.4
    (433 "Decode") ;; [TIFF-FX] 6.2.3
    (434 "DefaultImageColor") ;; [TIFF-FX] 8.2.3
    ;;435~512
    (513 "JPEGInterchangeFormat") ;; No mapping given
    (514 "JPEGInterchangeFormatLength") ;; No mapping given
    ;;515~528
    (529 "YCbCrCoefficients" tiff "YCbCrCoefficients" SeqRational)
    (530 "YCbCrSubSampling" tiff "YCbCrSubSampling" SeqInteger)
    (531 "YCbCrPositioning" tiff "YCbCrPositioning" Integer)
    (532 "ReferenceBlackWhite" tiff "ReferenceBlackWhite" SeqRational)
    ;;533~558
    (559 "StripRowCounts") ;; [TIFF-FX] 8.2.3
    ;;560~699
    (700 "XMPPacket") ;; [XMP3] 1.1.6 TIFF
    (33432 "Copyright" dc "rights" LangAlt)
    (34665 "ExifIFDPointer") ;; No mapping given
    (34732 "ImageLayer") ;; [TIFF-FX] 8.2.3
    (34853 "GPSInfoIFDPointer") ;; No mapping given
    (40965 "InteroperabilityIFDPointer") ;; No mapping given
    )
  "A list of tag information that may appear in 0th IFDs.

To access the contents, use functions beginning with xmp-tiff-tag-info-.")

(defun xmp-tiff-tag-info-get (tag tag-info-alist)
  (assq tag tag-info-alist))
(defun xmp-tiff-tag-info-tag (tag-info) (nth 0 tag-info))
(defun xmp-tiff-tag-info-name (tag-info) (nth 1 tag-info))
(defun xmp-tiff-tag-info-xmp-ns-prefix (tag-info) (nth 2 tag-info))
(defun xmp-tiff-tag-info-xmp-name (tag-info) (nth 3 tag-info))
(defun xmp-tiff-tag-info-xmp-type (tag-info) (nth 4 tag-info))
(defun xmp-tiff-tag-info-xmp-convinfo (tag-info) (nth 5 tag-info))

(defun xmp-tiff-tag-name (tag tag-info-alist)
  (xmp-tiff-tag-info-name (xmp-tiff-tag-info-get tag tag-info-alist)))

(defun xmp-tiff-0th-tag-name (tag)
  (xmp-tiff-tag-name tag xmp-tiff-0th-tags))

(provide 'xmp-tiff)
;;; xmp-tiff.el ends here
