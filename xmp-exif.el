;;; xmp-exif.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024 AKIYAMA Kouhei

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

;; Convert EXIF to XMP properties.

;;; References:

;; [EXIF3]
;; CIPA DC-008-2023,
;; Exchangeable image file format for digital still cameras: Exif Version 3.0,
;; https://www.cipa.jp/std/documents/download_j.html?DC-008-2023-J

;; [EXIFXMP2024]
;; CIPA DC-010-2024, Exif metadata for XMP,
;; https://www.cipa.jp/std/documents/download_j.html?CIPA_DC-010-2024_E

;; [TIFF6]
;; TIFF Revision 6.0 Final, June 1992, Adobe Systems Incorporated,
;; https://pdfa.org/norm-refs/TIFF6.pdf

;;; Code:

(require 'xmp-xml)
(require 'xmp-tiff)
(require 'xmp-file-reader)

;;;; Tag Information

;; TODO: Unify xmp-rdf:*? Dependencies are a problem. I don't want to (require 'xmp).
(defconst xmp-exif-rdf:
  (xmp-xml-ns-name "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
(defconst xmp-exif-rdf:li (xmp-xml-ename xmp-exif-rdf: "li"))
(defconst xmp-exif-rdf:Seq (xmp-xml-ename xmp-exif-rdf: "Seq"))
(defconst xmp-exif-rdf:Bag (xmp-xml-ename xmp-exif-rdf: "Bag"))
(defconst xmp-exif-rdf:Alt (xmp-xml-ename xmp-exif-rdf: "Alt"))
(defconst xmp-exif-rdf:Description (xmp-xml-ename xmp-exif-rdf: "Description"))
(defconst xmp-exif-rdf:about (xmp-xml-ename xmp-exif-rdf: "about"))

(defconst xmp-exif-ns-alist
  `((exif . ,(xmp-xml-ns-name "http://ns.adobe.com/exif/1.0/"))
    (exifEX . ,(xmp-xml-ns-name "http://cipa.jp/exif/1.0/"))
    (xmp . ,(xmp-xml-ns-name "http://ns.adobe.com/xap/1.0/"))
    (dc . ,(xmp-xml-ns-name "http://purl.org/dc/elements/1.1/"))
    (tiff . ,(xmp-xml-ns-name "http://ns.adobe.com/tiff/1.0/"))))

(defconst xmp-exif-exif-tags
  '(;; [EXIFXMP2024] 7.3 Properties for version related information
    (#x9000 "ExifVersion" exif "ExifVersion" Text)
    (#xA000 "FlashpixVersion" exif "FlashpixVersion" Text)
    ;; [EXIFXMP2024] 7.4 Properties for image data characteristics
    (#xA001 "ColorSpace" exif "ColorSpace" Integer)
    (#xA500 "Gamma" exifEX "Gamma" Rational)
    ;; [EXIFXMP2024] 7.5 Properties for image configuration
    (#x9101 "ComponentsConfiguration" exif "ComponentsConfiguration" SeqInteger)
    (#x9102 "CompressedBitsPerPixel" exif "CompressedBitsPerPixel" Rational)
    (#xA002 "PixelXDimension" exif "PixelXDimension" Integer)
    (#xA003 "PixelYDimension" exif "PixelYDimension" Integer)
    ;; [EXIFXMP2024] 7.6 Properties for user information
    (#x927C "MakerNote") ;; No mapping given
    (#x9286 "UserComment" exif "UserComment" LangAlt)
    ;; [EXIFXMP2024] 7.7 Properties for file information
    (#xA004 "RelatedSoundFile" exif "RelatedSoundFile" Text)
    ;; [EXIFXMP2024] 7.8 Properties for date and time
    (#x9003 "DateTimeOriginal" exif "DateTimeOriginal" Date (DSO #x9291 #x9011))
    ;;(#x9004 "DateTimeDigitized" xmp "CreateDate" Date) ;; [obsolete]
    (#x9004 "DateTimeDigitized" exif "DateTimeDigitized" Date (DSO #x9292 #x9012))
    (#x9010 "OffsetTime") ;; Merge to DateTime
    (#x9011 "OffsetTimeOriginal") ;; Merge to DateTimeOriginal
    (#x9012 "OffsetTimeDigitized") ;; Merge to DateTimeDigitized
    (#x9290 "SubSecTime") ;; Merge to DateTime
    (#x9291 "SubSecTimeOriginal") ;; Merge to DateTimeOriginal
    (#x9292 "SubSecTimeDigitized") ;; Merge to DateTimeDigitized
    ;; [EXIFXMP2024] 7.9 Properties for picture-taking conditions
    (#x829A "ExposureTime" exif "ExposureTime" Rational)
    (#x829D "FNumber" exif "FNumber" Rational)
    (#x8822 "ExposureProgram" exif "ExposureProgram" Integer)
    (#x8824 "SpectralSensitivity" exif "SpectralSensitivity" Text)
    (#x8827 "PhotographicSensitivity" exifEX "PhotographicSensitivity" Integer) ;; (Exif 2.3 or later)
    (#x8827 "ISOSpeedRatings" exif "ISOSpeedRatings" SeqInteger) ;; (till Exif 2.21) ;; [obsolete]
    (#x8828 "OECF" exif "OECF" OECF/SFR)
    (#x8830 "SensitivityType" exifEX "SensitivityType" Integer)
    (#x8831 "StandardOutputSensitivity" exifEX "StandardOutput-Sensitivity" Integer)
    (#x8832 "RecommendedExposureIndex" exifEX "RecommendedExposureIndex" Integer)
    (#x8833 "ISOSpeed" exifEX "ISOSpeed" Integer)
    (#x8834 "ISOSpeedLatitudeyyy" exifEX "ISOSpeedLatitudeyyy" Integer)
    (#x8835 "ISOSpeedLatitudezzz" exifEX "ISOSpeedLatitudezzz" Interger)
    (#x9201 "ShutterSpeedValue" exif "ShutterSpeedValue" Rational)
    (#x9202 "ApertureValue" exif "ApertureValue" Rational)
    (#x9203 "BrightnessValue" exif "BrightnessValue" Rational)
    (#x9204 "ExposureBiasValue" exif "ExposureBiasValue" Rational)
    (#x9205 "MaxApertureValue" exif "MaxApertureValue" Rational)
    (#x9206 "SubjectDistance" exif "SubjectDistance" Rational)
    (#x9207 "MeteringMode" exif "MeteringMode" Integer)
    (#x9208 "LightSource" exif "LightSource" Integer)
    (#x9209 "Flash" exif "Flash" Flash)
    (#x920A "FocalLength" exif "FocalLength" Rational)
    (#x9214 "SubjectArea" exif "SubjectArea" SeqInteger)
    (#xA20B "FlashEnergy" exif "FlashEnergy" Rational)
    (#xA20C "SpatialFrequencyResponse" exif "SpatialFrequencyResponse" OECF/SFR)
    (#xA20E "FocalPlaneXResolution" exif "FocalPlaneXResolution" Rational)
    (#xA20F "FocalPlaneYResolution" exif "FocalPlaneYResolution" Rational)
    (#xA210 "FocalPlaneResolutionUnit" exif "FocalPlaneResolutionUnit" Integer)
    (#xA214 "SubjectLocation" exif "SubjectLocation" SeqInteger)
    (#xA215 "ExposureIndex" exif "ExposureIndex" Rational)
    (#xA217 "SensingMethod" exif "SensingMethod" Integer)
    (#xA300 "FileSource" exif "FileSource" Integer)
    (#xA301 "SceneType" exif "SceneType" Integer)
    (#xA302 "CFAPattern" exif "CFAPattern" CFAPattern)
    (#xA401 "CustomRendered" exif "CustomRendered" Integer)
    (#xA402 "ExposureMode" exif "ExposureMode" Integer)
    (#xA403 "WhiteBalance" exif "WhiteBalance" Integer)
    (#xA404 "DigitalZoomRatio" exif "DigitalZoomRatio" Rational)
    (#xA405 "FocalLengthIn35mmFilm" exif "FocalLengthIn35mmFilm" Integer)
    (#xA406 "SceneCaptureType" exif "SceneCaptureType" Integer)
    (#xA407 "GainControl" exif "GainControl" Integer)
    (#xA408 "Contrast" exif "Contrast" Integer)
    (#xA409 "Saturation" exif "Saturation" Integer)
    (#xA40A "Sharpness" exif "Sharpness" Integer)
    (#xA40B "DeviceSettingDescription" exif "DeviceSettingDescription" DeviceSettings)
    (#xA40C "SubjectDistanceRange" exif "SubjectDistanceRange" Integer)
    (#xA460 "CompositeImage" exifEX "CompositeImage" Integer)
    (#xA461 "SourceImageNumberOfCompositeImage" exifEX "SourceImageNumberOfCompositeImage" SeqInteger)
    (#xA462 "SourceExposureTimesOfCompositeImage" exifEX "SourceExposureTimesOfCompositeImage" SourceExposureTimesOfCompositeImage)
    ;; [EXIFXMP2024] 7.10 Properties for shooting situation
    (#x9400 "Temperature" exifEX "Temperature" Rational)
    (#x9401 "Humidity" exifEX "Humidity" Rational)
    (#x9402 "Pressure" exifEX "Pressure" Rational)
    (#x9403 "WaterDepth" exifEX "WaterDepth" Rational)
    (#x9404 "Acceleration" exifEX "Acceleration" Rational)
    (#x9405 "CameraElevationAngle" exifEX "CameraElevationAngle" Rational)
    ;; [EXIFXMP2024] 7.11 Other properties
    (#xA420 "ImageUniqueID" exifEX "ImageUniqueID" GUID)
    (#xA420 "ImageUniqueID" exif "ImageUniqueID" Text) ;; [obsolete]
    (#xA430 "CameraOwnerName" exifEX "CameraOwnerName" ProperName)
    (#xA431 "BodySerialNumber" exifEX "BodySerialNumber" Text)
    (#xA432 "LensSpecification" exifEX "LensSpecification" SeqRational)
    (#xA433 "LensMake" exifEX "LensMake" Text)
    (#xA434 "LensModel" exifEX "LensModel" Text)
    (#xA435 "LensSerialNumber" exifEX "LensSerialNumber" Text)
    (#xA436 "ImageTitle" exifEX "ImageTitle" Text)
    (#xA437 "Photographer" exifEX "Photographer" Text)
    (#xA438 "ImageEditor" exifEX "ImageEditor" Text)
    (#xA439 "CameraFirmware" exifEX "CameraFirmware" Text)
    (#xA43A "RAWDevelopingSoftware" exifEX "RAWDevelopingSoftware" Text)
    (#xA43B "ImageEditingSoftware" exifEX "ImageEditingSoftware" Text)
    (#xA43C "MetadataEditingSoftware" exifEX "MetadataEditingSoftware" Text)
    ;; ?
    ;; (#xA005 "InteroperabilityIFDPointer") ;; No mapping given
    )
  "A list of tag information that may appear in EXIF IFDs.

To access the contents, use functions beginning with xmp-tiff-tag-info-.")

(defconst xmp-exif-gps-tags
  '(;; [EXIFXMP2024] 8.3 Properties for GPS information
    (#x0 "GPSVersionID" exif "GPSVersionID" Text)
    (#x1 "GPSLatitudeRef") ;; Merge to GPSLatitude
    (#x2 "GPSLatitude" exif "GPSLatitude" GPSCoordinate (GPS #x1))
    (#x3 "GPSLongitudeRef") ;; Merge to GPSLongitude
    (#x4 "GPSLongitude" exif "GPSLongitude" GPSCoordinate (GPS #x3))
    (#x5 "GPSAltitudeRef" exifEX "GPSAltitudeRef" Integer)
    (#x5 "GPSAltitudeRef" exif "GPSAltitudeRef" Integer) ;; [obsolete]
    (#x6 "GPSAltitude" exif "GPSAltitude" Rational)
    (#x7 "GPSTimeStamp" exif "GPSTimeStamp" Date (GPS #x1D))
    (#x8 "GPSSatellites" exif "GPSSatellites" Text)
    (#x9 "GPSStatus" exif "GPSStatus" Text)
    (#xA "GPSMeasureMode" exif "GPSMeasureMode" Integer)
    (#xB "GPSDOP" exif "GPSDOP" Rational)
    (#xC "GPSSpeedRef" exif "GPSSpeedRef" Text)
    (#xD "GPSSpeed" exif "GPSSpeed" Rational)
    (#xE "GPSTrackRef" exif "GPSTrackRef" Text)
    (#xF "GPSTrack" exif "GPSTrack" Rational)
    (#x10 "GPSImgDirectionRef" exif "GPSImgDirectionRef" Text)
    (#x11 "GPSImgDirection" exif "GPSImgDirection" Rational)
    (#x12 "GPSMapDatum" exif "GPSMapDatum" Text)
    (#x13 "GPSDestLatitudeRef") ;; Merge to GPSDestLatitude
    (#x14 "GPSDestLatitude" exif "GPSDestLatitude" GPSCoordinate (GPS #x13))
    (#x15 "GPSDestLongitudeRef") ;; Merge to GPSDestLongitude
    (#x16 "GPSDestLongitude" exif "GPSDestLongitude" GPSCoordinate (GPS #x15))
    (#x17 "GPSDestBearingRef" exif "GPSDestBearingRef" Text)
    (#x18 "GPSDestBearing" exif "GPSDestBearing" Rational)
    (#x19 "GPSDestDistanceRef" exif "GPSDestDistanceRef" Text)
    (#x1A "GPSDestDistance" exif "GPSDestDistance" Rational)
    (#x1B "GPSProcessingMethod" exif "GPSProcessingMethod" Text)
    (#x1C "GPSAreaInformation" exif "GPSAreaInformation" Text)
    (#x1D "GPSDateStamp") ;; Merge to GPSTimeStamp
    (#x1E "GPSDifferential" exif "GPSDifferential" Integer)
    (#x1F "GPSHPositioningError" exif "GPSHPositioning-Error" Rational))
  "A list of tag information that may appear in GPS Info IFDs.

To access the contents, use functions beginning with xmp-tiff-tag-info-.")

;;;; Convert to XMP

;;;;; Convert field values to a text

(defun xmp-exif-field-value-to-text (value)
  (when (stringp value)
    value))

(defun xmp-exif-field-value-to-integer-text (value)
  (when (integerp value)
    (number-to-string value)))

(defun xmp-exif-field-value-to-rational-text (value)
  ;; [EXIFXMP2024] A.2.4.6 Rational
  (when (and (consp value) (integerp (car value)) (integerp (cdr value)))
    (concat (number-to-string (car value)) "/" (number-to-string (cdr value)))))

(defun xmp-exif-date-time-to-xmp-date-text (date-time subsec-time offset-time)
  "Create a XMP date type text from EXIF values.
  DATE-TIME : YYYY:MM:DD HH:MM:SS
  OFFSET-TIME : [+-]HH:MM
  SUBSEC-TIME : [0-9]+"
  ;; [EXIF3] 4.6.5.4.5. DateTime
  ;; [EXIF3] 4.6.6.6.6. SubsecTime
  ;; [EXIF3] 4.6.6.6.3. OffsetTime
  ;; [EXIFXMP2024] A.2.2.2 Date
  (when (and (stringp date-time)
             (string-match "\\`\\([0-9][0-9][0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\) \\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\) *\\'" date-time))
    (let ((ymd (concat (match-string 1 date-time) "-"
                       (match-string 2 date-time) "-"
                       (match-string 3 date-time) "T"
                       (match-string 4 date-time) ":"
                       (match-string 5 date-time) ":"
                       (match-string 6 date-time))))
      (concat
       ymd
       (when (and (stringp subsec-time)
                  (string-match "\\`\\([0-9]+\\) *\\'" subsec-time))
         (concat "." (match-string 1 subsec-time)))
       (when (and (stringp offset-time)
                  (string-match "\\`\\([+-][0-9][0-9]:[0-9][0-9]\\) *\\'" offset-time))
         (match-string 1 offset-time))))))
;; TEST: (xmp-exif-date-time-to-xmp-date-text "2024:10:09 18:34:18" "970" "+09:00") => "2024-10-09T18:34:18.970+09:00"
;; TEST: (xmp-exif-date-time-to-xmp-date-text "2024:10:09 18:34:18" nil "+09:00") => "2024-10-09T18:34:18+09:00"
;; TEST: (xmp-exif-date-time-to-xmp-date-text "2024:10:09 18:34:18" nil nil) => "2024-10-09T18:34:18" (time zone is unknown)

(defun xmp-exif-check-3-rationals (v)
  (and (consp v) (consp (cdr v)) (consp (cddr v)) (null (cdddr v))
       (integerp (caar v)) (integerp (caadr v)) (integerp (caaddr v))
       (integerp (cdar v)) (integerp (cdadr v)) (integerp (cdaddr v))))
;; TEST: (xmp-exif-check-3-rationals '((1 . 2) (3 . 4) (5 . 6))) => t
;; TEST: (xmp-exif-check-3-rationals '((1 . 2) (3 . 4) (5 . nil))) => nil
;; TEST: (xmp-exif-check-3-rationals '((1 . 2) (3 . 4) (5 . 6) (7 . 8))) => nil

(defun xmp-exif-gps-date-time-to-xmp-date-text (date time)
  ;; [EXIF3] 4.6.7.1.30. GPSDateStamp
  ;; [EXIF3] 4.6.7.1.8. GPSTimeStamp
  (let ((date-result
         (when (and (stringp date)
                    (string-match "\\`\\([0-9][0-9][0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\'" date))
           (concat (match-string 1 date) "-"
                   (match-string 2 date) "-"
                   (match-string 3 date))))
        (time-result
         (when (xmp-exif-check-3-rationals time)
           (when-let* ((sec-f (cl-loop for w in '(3600.0 60.0 1.0)
                                       for (n . d) in time
                                       when (<= d 0) return nil
                                       sum (/ (* w n) d)))
                       (sec (min (1- (* 24 60 60 1000))
                                 (max 0 (round (* sec-f 1000))))))
             (format "T%02d:%02d:%02d.%03dZ"
                     (/ sec 3600000)
                     (% (/ sec 60000) 60)
                     (% (/ sec 1000) 60)
                     (% sec 1000))))))
    (when date-result
      (concat date-result time-result))))
;; TEST: (xmp-exif-gps-date-time-to-xmp-date-text "2024:12:31" '((23 . 1) (45 . 1) (59 . 1))) => "2024-12-31T23:45:59.000Z"
;; TEST: (xmp-exif-gps-date-time-to-xmp-date-text "2024:12:31" '((23 . 1) (59 . 1) (59123 . 1000))) => "2024-12-31T23:59:59.123Z"
;; TEST: (xmp-exif-gps-date-time-to-xmp-date-text "2024:12:31" '((230 . 10) (590 . 10) (5999999 . 100000))) => "2024-12-31T23:59:59.999Z"
;; TEST: (xmp-exif-gps-date-time-to-xmp-date-text "2024:12:31" '((23 . 0) (45 . 1) (59 . 1))) => "2024-12-31"

(defun xmp-exif-gps-coord-to-xmp-text (dms ref)
  ;; [EXIF3] 4.6.7.1.2. GPSLatitudeRef
  ;; [EXIF3] 4.6.7.1.3. GPSLatitude
  ;; [EXIF3] 4.6.7.1.4. GPSLongitudeRef
  ;; [EXIF3] 4.6.7.1.5. GPSLongitude
  ;; [EXIF3] 4.6.7.1.20. GPSDestLatitudeRef
  ;; [EXIF3] 4.6.7.1.21. GPSDestLatitude
  ;; [EXIF3] 4.6.7.1.22. GPSDestLongitudeRef
  ;; [EXIF3] 4.6.7.1.23. GPSDestLongitude
  ;; [EXIFXMP2024] A.2.4.4 GPSCoordinate
  (when (and (stringp ref)
             (string-match-p "\\`[NSWE]\\'" ref)
             (xmp-exif-check-3-rationals dms))
    (when-let ((dms-str
                (cl-loop for (n . d) in dms
                         for i from 0
                         when (<= d 0) return nil
                         unless (and (= i 2) (= n 0))
                         concat (concat
                                 (when (> i 0) ",")
                                 (cond
                                  ((= d 1)
                                   (number-to-string n))
                                  (t
                                   (format "%s" (/ (float n) d))))))))
      (concat dms-str ref))))
;; (xmp-exif-gps-coord-to-xmp-text '((11 . 1) (22 . 1) (33 . 1)) "N") => "11,22,33N"
;; (xmp-exif-gps-coord-to-xmp-text '((11 . 1) (2345 . 100) (0 . 1)) "W") => "11,23.45W"
;; (xmp-exif-gps-coord-to-xmp-text '((123456 . 10000) (0 . 1) (0 . 1)) "S") => "12.3456,0S"

;;;;; Convert a field to an XML element

(defun xmp-exif-make-prop-elem-text (ename text)
  (when (stringp text)
    (xmp-xml-element ename nil (list text))))

(defun xmp-exif-field-to-elem-text (field reader tiff-header ename &rest _)
  (xmp-exif-make-prop-elem-text
   ename
   (xmp-exif-field-value-to-text
    (xmp-tiff-field-value-single field reader tiff-header))))

(defun xmp-exif-field-to-elem-integer (field reader tiff-header ename &rest _)
  (xmp-exif-make-prop-elem-text
   ename
   (xmp-exif-field-value-to-integer-text
    (xmp-tiff-field-value-single field reader tiff-header))))

(defun xmp-exif-field-to-elem-rational (field reader tiff-header ename &rest _)
  (xmp-exif-make-prop-elem-text
   ename
   (xmp-exif-field-value-to-rational-text
    (xmp-tiff-field-value-single field reader tiff-header))))

(defun xmp-exif-field-to-elem-array (field reader tiff-header ename
                                           array-type
                                           converter)
  (when-let* ((values (xmp-tiff-field-values field reader tiff-header))
              (items (cl-loop for value in values
                              for node = (funcall converter value)
                              when node
                              collect (xmp-xml-element xmp-exif-rdf:li nil
                                                       (list node)))))
    (xmp-xml-element ename nil
                     (list (xmp-xml-element array-type nil items)))))

(defun xmp-exif-field-to-elem-bag-text (field reader tiff-header ename &rest _)
  (xmp-exif-field-to-elem-array field reader tiff-header ename
                                xmp-exif-rdf:Bag
                                #'xmp-exif-field-value-to-text))

(defun xmp-exif-field-to-elem-seq-text (field reader tiff-header ename &rest _)
  (xmp-exif-field-to-elem-array field reader tiff-header ename
                                xmp-exif-rdf:Seq
                                #'xmp-exif-field-value-to-text))

(defun xmp-exif-field-to-elem-seq-integer-text (field reader tiff-header ename
                                                      &rest _)
  (xmp-exif-field-to-elem-array field reader tiff-header ename
                                xmp-exif-rdf:Seq
                                #'xmp-exif-field-value-to-integer-text))

(defun xmp-exif-field-to-elem-seq-rational-text (field reader tiff-header ename
                                                       &rest _)
  (xmp-exif-field-to-elem-array field reader tiff-header ename
                                xmp-exif-rdf:Seq
                                #'xmp-exif-field-value-to-rational-text))

(defun xmp-exif-field-to-elem-lang-alt (field reader tiff-header ename
                                              &rest _)
  ;; TODO: Support UNDEFINED type (See: [EXIF3] 4.6.6.4.2.UserComment)
  (when-let ((text (xmp-exif-field-value-to-text
                    (xmp-tiff-field-value-single field reader tiff-header))))
    (xmp-xml-element
     ename nil
     (list (xmp-xml-element
            xmp-exif-rdf:Alt nil
            (list (xmp-xml-element
                   xmp-exif-rdf:li
                   (list (xmp-xml-attr xmp-xml:lang "x-default"))
                   (list text))))))))

(defun xmp-exif-field-to-elem-date-text (field reader tiff-header ename
                                               tag-info fields extra-fields)
  ;; 306:DateTime,36880:OffsetTime,37520:SubSecTime
  ;;   => xmp:ModifyDate
  ;; 36867:DateTimeOriginal,36881:OffsetTimeOriginal,37521:SubSecTimeOriginal
  ;;   => exif:DateTimeOriginal
  ;; 36868:DateTimeDigitized,36882:OffsetTimeDigitized,37522:SubSecTimeDigitized
  ;;   => exif:DateTimeDigitized
  ;; 7:GPSTimeStamp(3Rationals),29:GPSDateStamp(YYYY:MM:DD)
  ;;   => exif:GPSTimeStamp
  (xmp-exif-make-prop-elem-text
   ename
   (pcase (xmp-tiff-tag-info-xmp-convinfo tag-info)
     ;; DateTime(String), Subsec(String), Offset(String)
     (`(DSO ,tag-subsec ,tag-offset)
      (let ((field-subsec (or (xmp-tiff-field-get tag-subsec fields t)
                              (xmp-tiff-field-get tag-subsec extra-fields t)))
            (field-offset (or (xmp-tiff-field-get tag-offset fields t)
                              (xmp-tiff-field-get tag-offset extra-fields t))))
        (xmp-exif-date-time-to-xmp-date-text
         (xmp-tiff-field-value-single field reader tiff-header)
         (and field-subsec
              (xmp-tiff-field-value-single field-subsec reader tiff-header))
         (and field-offset
              (xmp-tiff-field-value-single field-offset reader tiff-header)))))
     ;; GPSDate(String), GPSTime(3-Rationals)
     (`(GPS ,tag-date)
      (when-let ((field-date (xmp-tiff-field-get tag-date fields t)))
        (xmp-exif-gps-date-time-to-xmp-date-text
         (xmp-tiff-field-value-single field-date reader tiff-header)
         (xmp-tiff-field-values field reader tiff-header)))))))

(defun xmp-exif-field-to-elem-gps-coordinate (field reader tiff-header ename
                                                    tag-info fields
                                                    _extra-fields)
  (xmp-exif-make-prop-elem-text
   ename
   (pcase (xmp-tiff-tag-info-xmp-convinfo tag-info)
     (`(GPS ,tag-ref)
      (when-let ((field-ref (xmp-tiff-field-get tag-ref fields t)))
        (xmp-exif-gps-coord-to-xmp-text
         (xmp-tiff-field-values field reader tiff-header)
         (xmp-tiff-field-value-single field-ref reader tiff-header)))))))

(defconst xmp-exif-xmp-type-converter-alist
  '((Text . xmp-exif-field-to-elem-text)
    (GUID . xmp-exif-field-to-elem-text)
    (ProperName . xmp-exif-field-to-elem-text)
    (AgentName . xmp-exif-field-to-elem-text) ;; Convert format?
    (Integer . xmp-exif-field-to-elem-integer)
    (Rational . xmp-exif-field-to-elem-rational)
    (BagText . xmp-exif-field-to-elem-bag-text)
    (SeqText . xmp-exif-field-to-elem-seq-text)
    (SeqInteger . xmp-exif-field-to-elem-seq-integer-text)
    (SeqRational . xmp-exif-field-to-elem-seq-rational-text)
    (LangAlt . xmp-exif-field-to-elem-lang-alt)
    ;; (UNDEFINED to)LangAlt ;; TODO: Support
    (Date . xmp-exif-field-to-elem-date-text)
    (GPSCoordinate . xmp-exif-field-to-elem-gps-coordinate)
    ;; OECF/SFR
    ;; Flash
    ;; CFAPattern
    ;; DeviceSettings
    ;; SourceExposureTimesOfCompositeImage
    ))

(defun xmp-exif-field-to-xmp-property-element (field reader tiff-header
                                                     tag-info-alist
                                                     fields extra-fields)
  "Convert the FIELD to an XMP property element.

READER and TIFF-HEADER are used to read the value data.

TAG-INFO-ALIST is a list that holds tag information. If FIELD is in a
0th IFD, specify `xmp-tiff-0th-tags'. If FIELD is in an EXIF IFD,
specify `xmp-exif-exif-tags'. If FIELD is in an GPS Info IFD, specify
`xmp-exif-gps-tags'.

FIELDS and EXTRA-FIELDS are used when the value conversion process needs
to refer to other fields. FIELDS is a list of fields that FIELD is
included in. EXTRA-FIELDS is a list of fields in the EXIF IFD when
FIELDS is the 0th IFD. For example, when converting DateTime in the 0th
IFD, it is necessary to refer to SubSecTime and OffsetTime in the EXIF
IFD."
  (when-let* ((tag (xmp-tiff-field-tag field))
              (tag-info (xmp-tiff-tag-info-get tag tag-info-alist))
              (xmp-ns-prefix (xmp-tiff-tag-info-xmp-ns-prefix tag-info))
              (xmp-ns-name (alist-get xmp-ns-prefix xmp-exif-ns-alist))
              (xmp-name (xmp-tiff-tag-info-xmp-name tag-info))
              (xmp-type (xmp-tiff-tag-info-xmp-type tag-info))
              (converter (alist-get xmp-type
                                    xmp-exif-xmp-type-converter-alist))
              (element (funcall
                        converter field reader tiff-header
                        (xmp-xml-ename xmp-ns-name xmp-name)
                        tag-info fields extra-fields)))
    element))

;;;;; Fields to elements

(defun xmp-exif-fields-to-xmp-property-elements (fields
                                                 reader tiff-header
                                                 tag-info-alist
                                                 &optional
                                                 extra-fields)
  "Convert FIELDS to XMP property elements.

FIELDS is a list of field information read from IFD.

Use READER and tiff-header to read specific field values from the field
information.

TAG-INFO-ALIST contains mapping information between EXIF tags and XMP
properties.

EXTRA-FIELDS is a field information list for obtaining additional
information. For example, when converting DateTime(#306) to XMP Date
type, the 0th IFD alone cannot obtain the time difference or fractional
seconds. Therefore, if FIELDS is a field list obtained from the 0th IFD,
EXTRA-FIELDS must also pass the field list obtained from the EXIF IFD."
  (cl-loop for field in fields
           for element = (xmp-exif-field-to-xmp-property-element
                          field reader tiff-header tag-info-alist
                          fields extra-fields)
           when element
           collect element))

;;;; Read EXIF as XMP property elements

(defun xmp-exif-prop-ename-list-to-tag-list (prop-ename-list tag-info-alist)
  "Convert a list of property expanded names to a list of tag numbers."
  (cl-loop for prop-ename in prop-ename-list
           for prop-ns = (xmp-xml-ename-ns prop-ename)
           for prop-local = (xmp-xml-ename-local prop-ename)
           for tag = (cl-loop for tag-info in tag-info-alist
                              when (and
                                    (equal
                                     (xmp-tiff-tag-info-xmp-name tag-info)
                                     prop-local)
                                    (equal
                                     ;; TODO: Use xmp-predefined-namespace-prefix?
                                     (alist-get
                                      (xmp-tiff-tag-info-xmp-ns-prefix tag-info)
                                      xmp-exif-ns-alist)
                                     prop-ns))
                              return (xmp-tiff-tag-info-tag tag-info))
           when tag
           collect tag))
;; TEST: (xmp-exif-prop-ename-list-to-tag-list (list xmp-dc:creator xmp-xmp:ModifyDate) xmp-tiff-0th-tags) => (315 306)

(defun xmp-exif-read-exif-as-xmp-property-elements (reader &optional
                                                           prop-ename-list)
  "Read EXIF from READER and return a list of XMP property elements.

PROP-ENAME-LIST is the list of XMP property expanded names to read, nil
means all properties."
  (let* ((tiff-header (xmp-tiff-read-header reader))
         (0th-fields (xmp-tiff-read-0th-ifd
                      reader tiff-header
                      (if prop-ename-list
                          (nconc
                           ;; Always read ExifIFDPointer and GPSInfoIFDPointer
                           (list xmp-tiff-tag-exif-ifd
                                 xmp-tiff-tag-gps-info-ifd)
                           (xmp-exif-prop-ename-list-to-tag-list
                            prop-ename-list xmp-tiff-0th-tags))
                        'all)))
         (field-exif-ptr (xmp-tiff-field-get xmp-tiff-tag-exif-ifd
                                             0th-fields t))
         (exif-fields (when field-exif-ptr
                        (xmp-tiff-read-pointed-ifd
                         reader tiff-header
                         field-exif-ptr
                         (if prop-ename-list
                             (xmp-exif-prop-ename-list-to-tag-list
                              prop-ename-list xmp-exif-exif-tags)
                           'all))))
         (field-gps-ptr (xmp-tiff-field-get xmp-tiff-tag-gps-info-ifd
                                            0th-fields t))
         (gps-fields (when field-gps-ptr
                       (xmp-tiff-read-pointed-ifd
                        reader tiff-header field-gps-ptr
                        (if prop-ename-list
                            (xmp-exif-prop-ename-list-to-tag-list
                             prop-ename-list xmp-exif-gps-tags)
                          'all)))))

    (nconc
     (xmp-exif-fields-to-xmp-property-elements 0th-fields reader tiff-header
                                               xmp-tiff-0th-tags
                                               exif-fields)
     (xmp-exif-fields-to-xmp-property-elements exif-fields reader tiff-header
                                               xmp-exif-exif-tags)
     (xmp-exif-fields-to-xmp-property-elements gps-fields reader tiff-header
                                               xmp-exif-gps-tags))))

(defun xmp-exif-read-exif-as-xmp-property-elements-from-bytes (bytes
                                                               &optional
                                                               prop-ename-list)
  "Same as `xmp-exif-read-exif-as-xmp-property-elements', but reads from a
BYTES instead of a file reader."
  (with-temp-buffer
    (insert bytes)
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (let ((xmp-file-reader-keep-passed-region-p t)
          (reader (xmp-file-reader-open-current-buffer)))
      (xmp-exif-read-exif-as-xmp-property-elements reader prop-ename-list))))


(defun xmp-exif-read-exif-as-description-element (reader
                                                  &optional prop-ename-list)
  (when-let ((prop-elems (xmp-exif-read-exif-as-xmp-property-elements
                          reader prop-ename-list)))
    (xmp-xml-element
     xmp-exif-rdf:Description
     (list (xmp-xml-attr xmp-exif-rdf:about ""))
     prop-elems)))

(defun xmp-exif-read-exif-as-description-element-from-bytes (bytes
                                                             &optional
                                                             prop-ename-list)
  (with-temp-buffer
    (insert bytes)
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (let ((xmp-file-reader-keep-passed-region-p t)
          (reader (xmp-file-reader-open-current-buffer)))
      (xmp-exif-read-exif-as-description-element reader prop-ename-list))))
;; EXAMPLE:
;; (xmp-xml-print
;;  (current-buffer)
;;  (xmp-exif-read-exif-as-description-element-from-bytes
;;   (alist-get 'exif (xmp-file-scan-jpeg-app1 "test/xmp-test-uzumaki.jpg" (list xmp-file-scan-jpeg-exif-signature-info))))
;;  xmp-default-ns-name-prefix-alist)


;;;;; Dump for debugging

(defun xmp-exif-dump-format-tag (tag tag-info-alist)
  (if-let ((tag-name (xmp-tiff-tag-name tag tag-info-alist)))
      (format "%d:%s" tag tag-name)
    tag))

(defun xmp-exif-dump-format-type (type)
  (pcase type
    (1 'BYTE)
    (2 'ASCII)
    (3 'SHORT)
    (4 'LONG)
    (5 'RATIONAL)
    (6 'SBYTE)
    (7 'UNDEFINED)
    (8 'SSHORT)
    (9 'SLONG)
    (10 'SRATIONAL)
    (11 'FLOAT)
    (12 'DOUBLE)
    (129 'UTF-8)
    (_ (format "%d:INVALID-TYPE" type))))

(defun xmp-exif-dump-pointed-ifd (reader tiff-header field tag-info-alist)
  (xmp-exif-dump-ifd reader tiff-header
                     (xmp-tiff-field-value-single field reader tiff-header)
                     tag-info-alist))

(defun xmp-exif-dump-ifd (reader tiff-header ifd-offset-from-tiff-header
                                 tag-info-alist)
  (cl-loop for field in (xmp-tiff-seek-and-read-ifd reader tiff-header
                                                    ifd-offset-from-tiff-header
                                                    'all)
           for tag = (xmp-tiff-field-tag field)
           for type = (xmp-tiff-field-type field)
           for count = (xmp-tiff-field-value-count field)
           for values =
           (cond
            ((eq tag xmp-tiff-tag-exif-ifd)
             (xmp-exif-dump-pointed-ifd reader tiff-header field
                                        xmp-exif-exif-tags))
            ((eq tag xmp-tiff-tag-gps-info-ifd)
             (xmp-exif-dump-pointed-ifd reader tiff-header field
                                        xmp-exif-gps-tags))
            (t
             (xmp-tiff-field-values field reader tiff-header)))
           collect (list (xmp-exif-dump-format-tag tag tag-info-alist)
                         (xmp-exif-dump-format-type type)
                         count values)))

(defun xmp-exif-dump (reader)
  (let ((tiff-header (xmp-tiff-read-header reader)))
    (xmp-exif-dump-ifd reader
                       tiff-header
                       (xmp-tiff-header-0th-ifd-offset tiff-header)
                       xmp-tiff-0th-tags)))

(defun xmp-exif-dump-from-bytes (bytes)
  (with-temp-buffer
    (insert bytes)
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (let ((xmp-file-reader-keep-passed-region-p t)
          (reader (xmp-file-reader-open-current-buffer)))
      (xmp-exif-dump reader))))
;; EXAMPLE: (xmp-exif-dump-from-bytes (alist-get 'exif (xmp-file-scan-jpeg-app1 "test/xmp-test-uzumaki.jpg" (list xmp-file-scan-jpeg-exif-signature-info))))

(defun xmp-exif-dump-from-tiff-file (tiff-file)
  (with-temp-buffer
    (let ((reader (xmp-file-reader-open tiff-file)))
      (xmp-exif-dump reader))))

(provide 'xmp-exif)
;;; xmp-exif.el ends here
