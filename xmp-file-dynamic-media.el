;;; xmp-file-dynamic-media.el ---                   -*- lexical-binding: t; -*-

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

;; [XMP3] Extensible Metadata Platform (XMP) Specification Part3 [Jan, 2020]
;;        https://developer.adobe.com/xmp/docs/XMPSpecifications/

;; [ID3V1] https://id3.org/ID3v1
;; [ID3V20] https://id3.org/id3v2-00
;; [ID3V23] https://id3.org/id3v2.3.0
;; [ID3V24S] https://id3.org/id3v2.4.0-structure
;; [ID3V24F] https://id3.org/id3v2.4.0-frames

;;; Code:

(require 'xmp)
(require 'xmp-file-reader)

;;;; Convert metadata to XMP

(defconst xmp-dm-property-conversion-info-list
  ;; [XMP3] 2.3.3 Native metadata in MP3
  ;; https://developer.adobe.com/xmp/docs/XMPNamespaces/xmpDM/
  ;; https://developer.adobe.com/xmp/docs/XMPNamespaces/dc/
  ;; https://developer.adobe.com/xmp/docs/XMPNamespaces/xmp/
  ;; https://developer.adobe.com/xmp/docs/XMPNamespaces/xmpRights/
  '((title         "dc:title" LangAlt)
    (artist        "xmpDM:artist" Text)
    (album         "xmpDM:album" Text)
    (date          "xmp:CreateDate" Date)
    (comment       "xmpDM:logComment" Text)
    (track-number  "xmpDM:trackNumber" Integer)
    (genre         "xmpDM:genre" Text)
    (composer      "xmpDM:composer" Text)
    (copyright     "dc:rights" LangAlt)
    (engineer      "xmpDM:engineer" Text)
    (disc-number   "xmpDM:discNumber" Text)
    ;; (compilation   "xmpDM:partOfCompilation" Boolean)
    ;; (lyrics        "xmpDM:lyrics" Text)
    ;; (web-copyright "xmpRights:WebStatement" Text)

    ;; TODO: support
    ;; (author "" ) ;; udta/auth
    ;; (description ) ;; udta/description
    ;; (keywords )  ;; udta/keywords
    ))

(defun xmp-dm-convert-property-alist-to-xmp-dom (props &optional
                                                       dom no-overwrite)
  (dolist (prop props)
    (let* ((prop-sym (car prop))
           (prop-val (cdr prop))
           (prop-info (assq prop-sym xmp-dm-property-conversion-info-list)))
      (when prop-info
        (let* ((prop-name (nth 1 prop-info))
               (prop-type (nth 2 prop-info))
               (prop-ename (ignore-errors
                             (xmp-xml-ename-from-prefixed-string prop-name))))
          (when (and prop-ename
                     ;; overwrite?
                     (not (and no-overwrite dom
                               (xmp-find-description dom prop-ename))))
            (let ((prop-pvalue (xmp-dm-convert-property-to-pvalue prop-val
                                                                  prop-type
                                                                  prop-sym)))
              (when prop-pvalue
                (unless dom
                  (setq dom (xmp-empty-dom)))
                (xmp-set-property-value dom prop-ename prop-pvalue))))))))
  dom)

(defun xmp-dm-convert-property-to-pvalue (prop-val prop-type _prop-sym)
  (pcase prop-type
    ('Text (xmp-pvalue-make-text prop-val))
    ('Date (xmp-pvalue-make-text prop-val))
    ('LangAlt (xmp-pvalue-make-lang-alt-x-default prop-val))
    ('Integer
     ;; Track number: n/m => n
     (when (string-match "\\`\\(-?[0-9]+\\)" prop-val)
       (xmp-pvalue-make-text (match-string 1 prop-val))))))


;;;; ID3

(defconst xmp-id3-verbose nil)

(defconst xmp-id3-enabled-major-versions '(1 2 3 4)
  "List of ID3 versions to process.
I haven't tested ID3v2.2 because I don't have the files to hand.")

(defun xmp-id3-major-version-enabled-p (ver)
  (memq ver xmp-id3-enabled-major-versions))

;; Symbol       v2.2 v2.3 XMP
;; -------------+---+----+-------------------------------
;; title         TT2 TIT2 dc:title["x-default"]
;; artist        TP1 TPE1 xmpDM:artist
;; album         TAL TALB xmpDM:album
;; date          (TYE,TDA,TIM) (TYER,TDAT,TIME | v2.4:TDRC) xmp:CreateDate
;; comment       COM COMM xmpDM:logComment
;; track-number  TRK TRCK xmpDM:trackNumber
;; genre         TCO TCON xmpDM:genre
;; composer      TCM TCOM xmpDM:composer
;; copyright     TCR TCOP dc:rights["x-default"]
;; engineer      TP4 TPE4 xmpDM:engineer
;; disc-number   TPA TPOS xmpDM:discNumber
;; compilation   TCP TCMP xmpDM:partOfCompilation
;; lyrics        ULT USLT xmpDM:lyrics
;; web-copyright WCP WCOP xmpRights:WebStatement

(defun xmp-id3-read-file (file)
  (with-temp-buffer
    (let ((reader (xmp-file-reader-open file)))
      (when (xmp-file-reader-ensure-bytes reader 3 'noerror)
        (if (string= (xmp-file-reader-scan-bytes reader 3) "ID3")
            (xmp-id3-v2-read reader)
          (when (xmp-id3-major-version-enabled-p 1)
            (let ((file-size (file-attribute-size (file-attributes file))))
              (when (and file-size (>= file-size 128))
                (xmp-file-reader-seek reader (- file-size 128))
                (xmp-id3-v1-read reader)))))))))

;;;;; Convert to XMP

(defun xmp-id3-read-file-as-xmp-dom (file)
  (xmp-dm-convert-property-alist-to-xmp-dom
   (xmp-id3-read-file file)))

;;;;; ID3v1

(defun xmp-id3-v1-read (reader)
  (when (string= (xmp-file-reader-read-bytes reader 3) "TAG")
    (let* ((props
            (delq nil
                  (list
                   (xmp-id3-v1-read-string 'title reader 30)
                   (xmp-id3-v1-read-string 'artist reader 30)
                   (xmp-id3-v1-read-string 'album reader 30)
                   (xmp-id3-v1-read-string 'date reader 4)
                   (xmp-id3-v1-read-album-track 'track-number reader)
                   (xmp-id3-v1-read-string 'comment reader 30)
                   (xmp-id3-v1-read-genre 'genre reader))))
           (coding-system (xmp-id3-v1-detect-coding-system
                           (mapconcat #'cdr props))))
      ;; Decode character encoding
      (dolist (prop props)
        (setcdr prop (decode-coding-string (cdr prop) coding-system)))
      props)))

(defun xmp-id3-v1-read-string (name reader size)
  (let ((value (string-trim-right
                (xmp-file-reader-read-asciiz-field reader size))))
    (unless (string-empty-p value)
      (cons name value))))

(defun xmp-id3-v1-read-album-track (name reader)
  ;; Read ahead from the end of the comment field
  (let* ((b (xmp-file-reader-scan-forward-bytes reader 28 2))
         (b0 (aref b 0))
         (b1 (aref b 1)))
    (when (and (= b0 0) (/= b1 0))
      (cons name
            (number-to-string b1)))))

(defconst xmp-id3-v1-genre-list
  [;; 0~
   "Blues" "Classic Rock" "Country" "Dance" "Disco"
   "Funk" "Grunge" "Hip-Hop" "Jazz" "Metal"
   "New Age" "Oldies" "Other" "Pop" "R&B"
   "Rap" "Reggae" "Rock" "Techno" "Industrial"
   "Alternative" "Ska" "Death Metal" "Pranks" "Soundtrack"
   "Euro-Techno" "Ambient" "Trip-Hop" "Vocal" "Jazz+Funk"
   "Fusion" "Trance" "Classical" "Instrumental" "Acid"
   "House" "Game" "Sound Clip" "Gospel" "Noise"
   "AlternRock" "Bass" "Soul" "Punk" "Space"
   "Meditative" "Instrumental Pop" "Instrumental Rock" "Ethnic" "Gothic"
   ;; 50~
   "Darkwave" "Techno-Industrial" "Electronic" "Pop-Folk" "Eurodance"
   "Dream" "Southern Rock" "Comedy" "Cult" "Gangsta"
   "Top 40" "Christian Rap" "Pop/Funk" "Jungle" "Native American"
   "Cabaret" "New Wave" "Psychadelic" "Rave" "Showtunes"
   "Trailer" "Lo-Fi" "Tribal" "Acid Punk" "Acid Jazz"
   "Polka" "Retro" "Musical" "Rock & Roll" "Hard Rock"
   "Folk" "Folk-Rock" "National Folk" "Swing" "Fast Fusion"
   "Bebob" "Latin" "Revival" "Celtic" "Bluegrass"
   "Avantgarde" "Gothic Rock" "Progressive Rock" "Psychedelic Rock"
   "Symphonic Rock"
   "Slow Rock" "Big Band" "Chorus" "Easy Listening" "Acoustic"
   ;; 100~
   "Humour" "Speech" "Chanson" "Opera" "Chamber Music"
   "Sonata" "Symphony" "Booty Bass" "Primus" "Porn Groove"
   "Satire" "Slow Jam" "Club" "Tango" "Samba"
   "Folklore" "Ballad" "Power Ballad" "Rhythmic Soul" "Freestyle"
   "Duet" "Punk Rock" "Drum Solo" "A capella" "Euro-House"
   "Dance Hall" "Goa music" "Drum & Bass" "Club-House" "Hardcore Techno"
   "Terror" "Indie" "BritPop" "Negerpunk" "Polsk Punk"
   "Beat" "Christian Gangsta Rap" "Heavy Metal" "Black Metal" "Crossover"
   "Contemporary Christian" "Christian Rock" "Merengue" "Salsa" "Thrash Metal"
   "Anime" "Jpop" "Synthpop" "Abstract" "Art Rock"
   ;; 150~
   "Baroque" "Bhangra" "Big beat" "Breakbeat" "Chillout"
   "Downtempo" "Dub" "EBM" "Eclectic" "Electro"
   "Electroclash" "Emo" "Experimental" "Garage" "Global"
   "IDM" "Illbient" "Industro-Goth" "Jam Band" "Krautrock"
   "Leftfield" "Lounge" "Math Rock" "New Romantic" "Nu-Breakz"
   "Post-Punk" "Post-Rock" "Psytrance" "Shoegaze" "Space Rock"
   "Trop Rock" "World Music" "Neoclassical" "Audiobook" "Audio Theatre"
   "Neue Deutsche Welle" "Podcast" "Indie-Rock" "G-Funk" "Dubstep"
   "Garage Rock" "Psybient" ;; ~191
   ])

(defun xmp-id3-v1-genre-code-to-string (code)
  (concat
   (format "(%d)" code)
   (when (<= 0 code (1- (length xmp-id3-v1-genre-list)))
     (aref xmp-id3-v1-genre-list code))))

(defun xmp-id3-v1-read-genre (name reader)
  (when-let ((v (xmp-id3-v1-genre-code-to-string (xmp-file-reader-u8 reader))))
    (cons name v)))

(defun xmp-id3-v1-detect-coding-system (str)
  ;; TODO: Customize?
  (detect-coding-string str t))


;;;;; ID3v2

(defconst xmp-id3-v2-properties
  '((comment ("COM" "COMM"))
    (album ("TAL" "TALB"))
    (composer ("TCM" "TCOM"))
    (genre ("TCO" "TCON"))
    (copyright ("TCR" "TCOP"))
    (date ("TDA" "TYE" "TIM"  "TDAT" "TYER" "TIME"  "TDRC" "TDRL"))
    (title ("TT2" "TIT2"))
    (artist ("TP1" "TPE1"))
    (engineer ("TP4" "TPE4"))
    (disc-number ("TPA" "TPOS"))
    (track-number ("TRK" "TRCK"))
    ;; (compilation ("TCP" "TCMP"))
    ;; (lyrics ("ULT" "USLT"))
    ;; (web-copyright ("WCP" "WCOP"))
    ))

(defconst xmp-id3-v2-target-frame-ids
  (cl-loop for prop-info in xmp-id3-v2-properties
           append (nth 1 prop-info)))

(defun xmp-id3-v2-read (reader)
  (when-let ((tag-header (xmp-id3-v2-read-tag-header reader))
             (frame-values (xmp-id3-v2-read-frames
                            reader tag-header
                            xmp-id3-v2-target-frame-ids)))
    ;; Convert frames to properties
    (cl-loop for (name frame-ids) in xmp-id3-v2-properties
             for value =
             (pcase name
               ('date
                (xmp-id3-v2-prop-date frame-values frame-ids))
               (_
                (xmp-id3-v2-prop-text frame-values frame-ids)))
             when value
             collect (cons name value))))

(defun xmp-id3-v2-prop-text (frame-values frame-ids)
  (seq-some
   (lambda (fid)
     (alist-get fid frame-values nil nil #'string=))
   frame-ids))

(defun xmp-id3-v2-time-add-z (time)
  "Add Z to the end of the TIME to represent UTC.
All timestamps are in UTC (See URL`https://id3.org/id3v2.4.0-structure')."
  (if (and (stringp time)
           (string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]\\(?::[0-9][0-9]\\(?::[0-9][0-9]\\)?\\)?\\'" time))
      (concat time "Z")
    time))
;; TEST: (xmp-id3-v2-time-add-z "2024-12-05") => "2024-12-05"
;; TEST: (xmp-id3-v2-time-add-z "2024-12-05T12") => "2024-12-05T12Z"
;; TEST: (xmp-id3-v2-time-add-z "2024-12-05T12:34") => "2024-12-05T12:34Z"
;; TEST: (xmp-id3-v2-time-add-z "2024-12-05T12:34:50") => "2024-12-05T12:34:50Z"

(defun xmp-id3-v2-prop-date (frame-values _frame-ids)
  (or
   ;; v2.4
   (xmp-id3-v2-time-add-z
    (alist-get "TDRC" frame-values nil nil #'string=)) ;; Recording time
   (xmp-id3-v2-time-add-z
    (alist-get "TDRL" frame-values nil nil #'string=)) ;; Release time
   ;; v2.3 or 2.2
   (let ((year (or (alist-get "TYER" frame-values nil nil #'string=)
                   (alist-get "TYE" frame-values nil nil #'string=)))
         (date (or (alist-get "TDAT" frame-values nil nil #'string=)
                   (alist-get "TDA" frame-values nil nil #'string=)))
         (time (or (alist-get "TIME" frame-values nil nil #'string=)
                   (alist-get "TIM" frame-values nil nil #'string=))))
     (when (and (stringp year) (= (length year) 4))
       (concat
        year
        (when (and (stringp date) (= (length date) 4))
          (concat
           "-"
           (substring date 2 4)
           "-"
           (substring date 0 2)
           (when (and (stringp time) (= (length time) 4))
             (concat
              "T"
              (substring date 0 2)
              ":"
              (substring date 2 4))))))))))


(defun xmp-id3-v2-read-u28 (reader)
  "Read a 32-bit synchsafe integer from READER."
  (xmp-file-reader-ensure-bytes reader 4)
  (let ((pos (point)))
    (forward-char 4)
    (+ (ash (logand 127 (char-after pos)) 21)
       (ash (logand 127 (char-after (+ pos 1))) 14)
       (ash (logand 127 (char-after (+ pos 2))) 7)
       (logand 127 (char-after (+ pos 3))))))

(defun xmp-id3-v2-read-tag-header (reader)
  (when (string= (xmp-file-reader-read-bytes reader 3) "ID3")
    (let* ((major-version (xmp-file-reader-u8 reader))
           (minor-version (xmp-file-reader-u8 reader))
           (flags (xmp-file-reader-u8 reader))
           (body-size (xmp-id3-v2-read-u28 reader))
           (unsynchronised (/= (logand flags 128) 0))
           (compressed (and (= major-version 2)
                            (/= (logand flags 64) 0)))
           (extended (and (/= major-version 2)
                          (/= (logand flags 64) 0)))
           (experimental (/= (logand flags 32) 0))
           ;;(footer-present (/= (logand flags 16) 0))
           )
      (when (xmp-id3-major-version-enabled-p major-version)
        (unless (memq major-version '(2 3 4))
          (error "Unsupported ID3 version"))
        (when unsynchronised
          (error "Unsynchronisation is not supported"))
        (when compressed
          (error "Compression is not supported"))
        (when experimental
          (error "Experimental TAG"))
        ;; Skip extended header
        (when extended
          (let ((extended-size (if (= major-version 3)
                                   (xmp-file-reader-u32be reader)
                                 (xmp-id3-v2-read-u28 reader))))
            (when (< extended-size 6)
              (error "Extended header size too small"))
            (xmp-file-reader-skip reader extended-size)))
        ;; Return tag header data
        (vector major-version minor-version flags body-size)))))

(defun xmp-id3-v2-tag-header-major-version (tag-header) (aref tag-header 0))
(defun xmp-id3-v2-tag-header-minor-version (tag-header) (aref tag-header 1))
(defun xmp-id3-v2-tag-header-flags (tag-header) (aref tag-header 2))
(defun xmp-id3-v2-tag-header-body-size (tag-header) (aref tag-header 3))

;; Frame

(defun xmp-id3-v2-read-frame-header (reader tag-header)
  (let* ((ver (xmp-id3-v2-tag-header-major-version tag-header))
         (frame-id-length (if (eq ver 2) 3 4))
         (frame-min-length (if (eq ver 2) 6 10)))
    (when (xmp-file-reader-ensure-bytes reader frame-min-length 'noerror)
      (let ((frame-id (xmp-file-reader-read-bytes reader frame-id-length)))
        (when (seq-every-p (lambda (ch) (or (<= ?A ch ?Z) (<= ?0 ch ?9)))
                           frame-id)
          (vector
           frame-id
           ;; body-size
           (pcase ver
             (2 (xmp-file-reader-u24be reader))
             (3 (xmp-file-reader-u32be reader))
             (4 (xmp-id3-v2-read-u28 reader)))
           ;; flags
           (if (eq ver 2) 0 (xmp-file-reader-u16be reader))
           ver))))))

(defun xmp-id3-v2-frame-header-id (frame-header) (aref frame-header 0))
(defun xmp-id3-v2-frame-header-body-size (frame-header) (aref frame-header 1))
(defun xmp-id3-v2-frame-header-flags (frame-header) (aref frame-header 2))
(defun xmp-id3-v2-frame-header-ver (frame-header) (aref frame-header 3))
(defun xmp-id3-v2-frame-header-flag-p (frame-header v3 v4)
  (let ((flags (xmp-id3-v2-frame-header-flags frame-header)))
    (pcase (xmp-id3-v2-frame-header-ver frame-header)
      (3 (/= (logand flags v3) 0))
      (4 (/= (logand flags v4) 0)))))
(defun xmp-id3-v2-frame-grouped-p (frame-header)
  (xmp-id3-v2-frame-header-flag-p frame-header 32 64))
(defun xmp-id3-v2-frame-compressed-p (frame-header)
  (xmp-id3-v2-frame-header-flag-p frame-header 128 8))
(defun xmp-id3-v2-frame-encrypted-p (frame-header)
  (xmp-id3-v2-frame-header-flag-p frame-header 64 4))
(defun xmp-id3-v2-frame-unsynchronised-p (frame-header)
  (xmp-id3-v2-frame-header-flag-p frame-header 0 2))
(defun xmp-id3-v2-frame-data-length-p (frame-header)
  (xmp-id3-v2-frame-header-flag-p frame-header 0 1))

(defun xmp-id3-v2-read-frames (reader tag-header frame-id-list)
  (let (frame-id-value-alist frame-header)
    (while (setq frame-header (xmp-id3-v2-read-frame-header reader tag-header))
      (let* ((next-offset (+ (xmp-file-reader-current-offset reader)
                             (xmp-id3-v2-frame-header-body-size frame-header)))
             (frame-id (xmp-id3-v2-frame-header-id frame-header))
             (frame-value
              (when (member frame-id frame-id-list)
                (xmp-id3-v2-read-frame-value reader frame-header))))
        (when frame-value
          (push (cons frame-id frame-value) frame-id-value-alist))
        (xmp-file-reader-seek reader next-offset)))
    frame-id-value-alist))

(defun xmp-id3-v2-read-frame-value (reader frame-header)
  (let ((frame-id (xmp-id3-v2-frame-header-id frame-header)))
    (cond
     ((= (aref frame-id 0) ?T)
      (xmp-id3-v2-read-text-information-frame reader frame-header))
     ((or (string= frame-id "COM") (string= frame-id "COMM"))
      (xmp-id3-v2-read-comment-frame reader frame-header)))))

(defun xmp-id3-v2-read-frame-data (reader frame-header max-size)
  (let ((size (xmp-id3-v2-frame-header-body-size frame-header)))
    ;; TODO: Support compression
    ;; TODO: Support encryption
    ;; TODO: Support unsynchronisation
    (when xmp-id3-verbose
      (when (> size max-size)
        (message "Encoded frame size is too large"))
      (when (xmp-id3-v2-frame-compressed-p frame-header)
        (message "Compression is not supported"))
      (when (xmp-id3-v2-frame-encrypted-p frame-header)
        (message "Encryption is not supported"))
      (when (xmp-id3-v2-frame-unsynchronised-p frame-header)
        (message "Unsynchronisation is not supported"))
      (when (xmp-id3-v2-frame-data-length-p frame-header)
        (message "Data length is not supported")))
    (when (and (<= size max-size)
               (not (xmp-id3-v2-frame-compressed-p frame-header))
               (not (xmp-id3-v2-frame-encrypted-p frame-header))
               (not (xmp-id3-v2-frame-unsynchronised-p frame-header))
               (not (xmp-id3-v2-frame-data-length-p frame-header)))
      ;; Skip group identifier
      (when (xmp-id3-v2-frame-grouped-p frame-header)
        (xmp-file-reader-u8 reader)
        (cl-decf size))
      (when (and xmp-id3-verbose (> size max-size))
        (message "Decoded frame size is too large"))
      (when (<= size max-size)
        (xmp-file-reader-read-bytes reader size)))))

(defconst xmp-id3-v2-max-text-frame-size 4096)

(defun xmp-id3-v2-split-byte-strings (bytes encoding start)
  (let ((end (length bytes))
        (pos start)
        result)
    (pcase encoding
      ((or 0 3)
       (while (< pos end)
         (when (= (aref bytes pos) 0)
           (push (substring bytes start pos) result)
           (setq start (+ pos 1)))
         (cl-incf pos))
       (when (< start end)
         (push (substring bytes start end) result)))
      ((or 1 2)
       (while (< (1+ pos) end)
         (when (and (= (aref bytes pos) 0) (= (aref bytes (1+ pos)) 0))
           (push (substring bytes start pos) result)
           (setq start (+ pos 2)))
         (cl-incf pos 2))
       (when (< end pos)
         (cl-decf pos 2))
       (when (< start pos)
         (push (substring bytes start pos) result))))
    (nreverse result)))
;; TEST: (xmp-id3-v2-split-byte-strings "ABC\0DEF\0GHI" 0 1) => ("BC" "DEF" "GHI")
;; TEST: (xmp-id3-v2-split-byte-strings "ABC\0DEF\0GHI\0" 0 1) => ("BC" "DEF" "GHI")
;; EXAMPLE: (xmp-id3-v2-split-byte-strings "A\0B\0C\0\0\0D\0E\0F\0\0\0G\0H\0I\0J" 1 2)

(defun xmp-id3-v2-decode-string (bytes encoding)
  (pcase encoding
    (0 (decode-coding-string bytes (detect-coding-string bytes t)))
    (1 (decode-coding-string bytes 'utf-16))
    (2 (decode-coding-string bytes 'utf-16be))
    (3 (decode-coding-string bytes 'utf-8))
    (_ (decode-coding-string bytes (detect-coding-string bytes t)))))

(defun xmp-id3-v2-read-text-information-frame (reader frame-header)
  ;; <Header for 'Text information frame', ID: "T000" - "TZZZ",
  ;; excluding "TXXX" described in 4.2.6.>
  ;; Text encoding  $xx
  ;; Information    <text string(s) according to encoding>
  (when-let* ((data (xmp-id3-v2-read-frame-data reader frame-header
                                                xmp-id3-v2-max-text-frame-size))
              (encoding (aref data 0))
              (texts (mapcar (lambda (str)
                               (xmp-id3-v2-decode-string str encoding))
                             (xmp-id3-v2-split-byte-strings data encoding 1))))
    (mapconcat #'identity texts " / ")))

(defun xmp-id3-v2-read-comment-frame (reader frame-header)
  ;; <Header for 'Comment', ID: "COMM">
  ;; Text encoding  $xx
  ;; Language       $xx xx xx
  ;; Short content descrip. <text string according to encoding> $00 (00)
  ;; The actual text        <full text string according to encoding>
  (when-let* ((data (xmp-id3-v2-read-frame-data reader frame-header
                                                xmp-id3-v2-max-text-frame-size))
              (encoding (aref data 0))
              (lang (substring data 1 4))
              (texts (mapcar (lambda (str)
                               (xmp-id3-v2-decode-string str encoding))
                             (xmp-id3-v2-split-byte-strings data encoding 4))))
    (if (string-empty-p (car texts))
        (cadr texts)
      (mapconcat #'identity texts " : "))))


;;;; ISO base media file format

(defconst xmp-isobmff-verbose nil)

(defmacro xmp-isobmff-fourcc (str)
  (+ (ash (aref str 0) 24)
     (ash (aref str 1) 16)
     (ash (aref str 2) 8)
     (aref str 3)))
;; TEST: (xmp-isobmff-fourcc "moov") => 1836019574

(defmacro xmp-isobmff-uuid (str)
  ;; Remove spaces
  (setq str (replace-regexp-in-string "[^0-9A-Fa-f]+" "" str))
  (apply
   #'unibyte-string
   (cl-loop for i below 32 by 2
            collect (string-to-number (substring str i (+ 2 i)) 16))))
;; EXAMPLE: (xmp-isobmff-uuid "BE7ACFCB 97A942E8 9C719994 91E3AFAC")
;; EXAMPLE: (xmp-isobmff-uuid "{0537cdab-9d0c-4431-a72a-fa561f2a113e}")

;;;;; Objects
;;;;;; Box object

(defun xmp-isobmff-read-box (reader parent-box)
  (let ((file-offset (xmp-file-reader-current-offset reader))
        (size (xmp-file-reader-u32be reader))
        (type (xmp-file-reader-u32be reader)))
    (pcase size
      ;; box extends to end of file
      (0 (setq size (- (xmp-file-reader-file-size reader) file-offset)))
      ;; 64-bit size
      (1 (setq size (xmp-file-reader-u64be reader))))

    (when (= type (xmp-isobmff-fourcc "uuid"))
      (setq type (xmp-file-reader-read-bytes reader 16)))
    (list :file-offset file-offset :size size :type type :parent parent-box)))

(defun xmp-isobmff-box-level (box)
  (let ((level 0))
    (while box
      (cl-incf level)
      (setq box (plist-get box :parent)))
    level))

(defun xmp-isobmff-box-root (box)
  (let (parent)
    (while (setq parent (plist-get box :parent))
      (setq box parent))
    box))

(defun xmp-isobmff-box-parent (box)
  (plist-get box :parent))

(defun xmp-isobmff-box-set-property (box key value)
  (let ((cell (plist-member box key)))
    (if cell
        (setcar (cdr cell) value)
      ;; Note: Do not prepend using push etc.
      (nconc box (list key value)))))

(defun xmp-isobmff-box-get-property-cell-create (box key)
  (let ((cell (plist-member box key)))
    (unless cell
      (setq cell (list key nil))
      ;; Note: Do not prepend using push etc.
      (nconc box cell))
    cell))

(defun xmp-isobmff-box-end-offset (box)
  (+ (plist-get box :file-offset) (plist-get box :size)))

(defun xmp-isobmff-box-remaining-size (box reader)
  (+ (plist-get box :file-offset)
     (plist-get box :size)
     (- (xmp-file-reader-current-offset reader))))

(defun xmp-isobmff-box-skip-to-end (box reader)
  (xmp-file-reader-skip reader (xmp-isobmff-box-remaining-size box reader)))

(defun xmp-isobmff-box-type-string (box)
  (let ((type (plist-get box :type)))
    (cond
     ((integerp type)
      (string (logand (ash type -24) 255)
              (logand (ash type -16) 255)
              (logand (ash type -8) 255)
              (logand type 255)))
     ((stringp type)
      (cl-loop for i below (length type)
               concat (format "%02x" (aref type i)))))))

;;;;;; FullBox object

(defun xmp-isobmff-read-full-box (reader &optional base-box parent-box)
  (unless base-box
    (setq base-box (xmp-isobmff-read-box reader parent-box)))
  ;; Note: Do not prepend using push etc.
  (nconc
   base-box
   (list :version (xmp-file-reader-u8 reader)
         :flags (xmp-file-reader-u24be reader))))

;;;;;; FileTypeBox object

(defun xmp-isobmff-read-file-type-box (reader base-box)
  ;; Note: Do not prepend using push etc.
  (nconc
   base-box
   (list :major-brand (xmp-file-reader-u32be reader)
         :minor-version (xmp-file-reader-u32be reader)
         :compatible-brands
         (cl-loop repeat (/ (xmp-isobmff-box-remaining-size base-box reader) 4)
                  collect (xmp-file-reader-u32be reader)))))

;;;;;; Container Box

(defun xmp-isobmff-call-handler (handler box reader)
  (cond
   ((functionp handler)
    (funcall handler reader box))
   ((consp handler)
    (let* ((box-type (plist-get box :type))
           (type-fun (assoc box-type handler)))
      (when type-fun
        (funcall (cdr type-fun) reader box))))))

(defun xmp-isobmff-read-box-children (reader container-box handler)
  (let (child-values)
    (while (> (xmp-isobmff-box-remaining-size container-box reader) 0)
      (let ((box (xmp-isobmff-read-box reader container-box)))
        (when xmp-isobmff-verbose
          (message "%stype=%s size=%s"
                   (make-string (* 2 (1- (xmp-isobmff-box-level box))) ? )
                   (xmp-isobmff-box-type-string box)
                   (plist-get box :size)))
        (when-let* ((value (xmp-isobmff-call-handler handler box reader)))
          (push value child-values))
        (xmp-isobmff-box-skip-to-end box reader)))
    (xmp-isobmff-box-skip-to-end container-box reader)
    (when child-values
      (setq child-values (nreverse child-values))
      (xmp-isobmff-box-set-property container-box :child-values child-values))
    child-values))

;;;;; File

(defun xmp-isobmff-read-file-children (file handler &optional props)
  (with-temp-buffer
    (let* ((reader (xmp-file-reader-open file))
           (root-box (append
                      (list :file-offset 0
                            :size (xmp-file-reader-file-size reader)
                            :type nil
                            :parent nil)
                      props)))
      (xmp-isobmff-read-box-children reader root-box handler)
      root-box)))

;;;;; Read Metadata
;;  :udta-meta (album author copyright description genre artist title
;;              date keywords)
;;  :mdir-meta
;;  :xmp (:begin :end :bytes)
;;  :exif (:begin :end :bytes)

(defun xmp-isobmff-read-metadata-from-file (file)
  (xmp-isobmff-read-file-children
   file
   `((,(xmp-isobmff-fourcc "moov") . xmp-isobmff-read-moov)
     (,(xmp-isobmff-uuid "BE7ACFCB 97A942E8 9C719994 91E3AFAC")
      . xmp-isobmff-read-xmp)
     (,(xmp-isobmff-uuid "0537CDAB 9D0C4431 A72AFA56 1F2A113E")
      . xmp-isobmff-read-exif))))

(defun xmp-isobmff-read-moov (reader box)
  (xmp-isobmff-read-box-children
   reader
   box
   ;; TODO: Get creation_time and modification_time from mvhd box
   `(;;(,(xmp-isobmff-fourcc "trak") . xmp-isobmff-read-trak)
     (,(xmp-isobmff-fourcc "udta") . xmp-isobmff-read-udta))))

;; (defun xmp-isobmff-read-trak (reader box)
;;   (xmp-isobmff-read-box-children
;;    reader
;;    box
;;    `((,(xmp-isobmff-fourcc "udta") . xmp-isobmff-read-udta))))

(defun xmp-isobmff-read-udta (reader box)
  (xmp-isobmff-read-box-children
   reader
   box
   `((,(xmp-isobmff-fourcc "meta") . xmp-isobmff-read-udta-meta)
     (,(xmp-isobmff-fourcc "albm") . xmp-isobmff-read-udta-albm)
     (,(xmp-isobmff-fourcc "auth") . xmp-isobmff-read-udta-auth)
     (,(xmp-isobmff-fourcc "cprt") . xmp-isobmff-read-udta-cprt)
     (,(xmp-isobmff-fourcc "dscp") . xmp-isobmff-read-udta-dscp)
     (,(xmp-isobmff-fourcc "gnre") . xmp-isobmff-read-udta-gnre)
     (,(xmp-isobmff-fourcc "kywd") . xmp-isobmff-read-udta-kywd)
     (,(xmp-isobmff-fourcc "perf") . xmp-isobmff-read-udta-perf)
     (,(xmp-isobmff-fourcc "titl") . xmp-isobmff-read-udta-titl)
     (,(xmp-isobmff-fourcc "yrrc") . xmp-isobmff-read-udta-yrrc))))

(defun xmp-isobmff-read-hdlr (reader &optional box parent-box)
  (unless box
    (setq box (xmp-isobmff-read-full-box reader nil parent-box)))
  ;; https://exiftool.org/TagNames/QuickTime.html#Handler
  (when (eq (plist-get box :version) 0)
    (xmp-file-reader-skip reader 4) ;; pre_defined (HandlerClass?)
    (let ((handler-type (xmp-file-reader-u32be reader)))
      ;; Ignore
      ;; (xmp-file-reader-skip reader (* 4 3)) ;; reserved
      ;; (xmp-file-reader-read-utf8z reader) ;; name
      (xmp-isobmff-box-skip-to-end box reader)
      (xmp-isobmff-box-set-property box :handler-type handler-type))
    box))

(defun xmp-isobmff-read-udta-meta (reader box)
  (xmp-isobmff-read-full-box reader box)

  (when-let* ((handler-ref-box (xmp-isobmff-read-hdlr reader nil box)))
    (let ((handler-type (plist-get handler-ref-box :handler-type)))
      ;; https://exiftool.org/TagNames/QuickTime.html#Handler
      (when (equal handler-type (xmp-isobmff-fourcc "mdir"))
        (xmp-isobmff-read-box-children
         reader
         box
         `((,(xmp-isobmff-fourcc "ilst") . xmp-isobmff-read-mdir-ilst)))))))

;;;;;; 3GPP asset meta data

;; TODO: Support following meta data?
;; - Rating box (rtng)
;; - Classification box (clsf)
;; - location information box (loci)
;; - Collection name box (coll)
;; - User rating box (urat)
;; - Thumbnail box (thmb)
;; - Orientation information box (orie)
;; - ID3v2 meta box

(defun xmp-isobmff-read-udta-albm (reader box) ;; Album
  (xmp-isobmff-read-full-box reader box)
  (when (eq (plist-get box :version) 0)
    (xmp-isobmff-udta-meta-put-with-lang
     box
     (xmp-file-reader-u16be reader) ;; Language
     ;; AlbumTitle
     'album (xmp-file-reader-read-utf-8-or-16-z reader))
    ;; Optional track number
    (when (> (xmp-isobmff-box-remaining-size box reader) 0)
      (xmp-isobmff-udta-meta-put
       box
       'track-number (number-to-string (xmp-file-reader-u8 reader)))
      nil)))

(defun xmp-isobmff-read-udta-auth (reader box) ;; Author
  (xmp-isobmff-read-udta-meta-data-string reader box 'author))

(defun xmp-isobmff-read-udta-cprt (reader box) ;; Copyright
  (xmp-isobmff-read-udta-meta-data-string reader box 'copyright))

(defun xmp-isobmff-read-udta-dscp (reader box) ;; Description
  (xmp-isobmff-read-udta-meta-data-string reader box 'description))

(defun xmp-isobmff-read-udta-gnre (reader box) ;; Genre
  (xmp-isobmff-read-udta-meta-data-string reader box 'genre))

(defun xmp-isobmff-read-udta-perf (reader box) ;; Performer
  (xmp-isobmff-read-udta-meta-data-string reader box 'artist))

(defun xmp-isobmff-read-udta-titl (reader box) ;; Title
  (xmp-isobmff-read-udta-meta-data-string reader box 'title))

(defun xmp-isobmff-read-udta-yrrc (reader box) ;; Recording Year
  (xmp-isobmff-read-full-box reader box)
  (when (eq (plist-get box :version) 0)
    ;; RecordingYear
    (let ((year (xmp-file-reader-u16be reader)))
      (when (< 0 year 3000)
        (xmp-isobmff-udta-meta-put box 'date (number-to-string year))
        nil))))

(defun xmp-isobmff-read-udta-kywd (reader box)
  (xmp-isobmff-read-full-box reader box)
  (when (eq (plist-get box :version) 0)
    (xmp-isobmff-udta-meta-put-with-lang
     box
     (xmp-file-reader-u16be reader) ;; Language
     'keywords
     (cl-loop repeat (xmp-file-reader-u8 reader)
              collect
              (let* ((keyword-size (xmp-file-reader-u8 reader))
                     (keyword-offset (xmp-file-reader-current-offset reader))
                     (keyword-info (xmp-file-reader-read-utf-8-or-16-z reader))
                     (actual-size (- (xmp-file-reader-current-offset reader)
                                     keyword-offset)))
                (when (< actual-size keyword-size)
                  (xmp-file-reader-skip reader (- keyword-size actual-size)))
                keyword-info)))
    nil))

(defun xmp-isobmff-read-udta-meta-data-string (reader box key)
  "Common implementation for reading simple string from ISOBMFF asset meta."
  (xmp-isobmff-read-full-box reader box)
  (when (eq (plist-get box :version) 0)
    (xmp-isobmff-udta-meta-put-with-lang
     box
     (xmp-file-reader-u16be reader) ;; Language
     key (xmp-file-reader-read-utf-8-or-16-z reader))
    nil))

(defun xmp-isobmff-udta-meta-put-with-lang (box _lang key value)
  ;; TODO: Extract LANG
  ;; Language: declares the language code for the following text. See
  ;; ISO 639-2/T for the set of three character codes.  Each character
  ;; is packed as the difference between its ASCII value and 0x60. The
  ;; code is confined to being three lower-case letters, so these
  ;; values are strictly positive.
  (xmp-isobmff-udta-meta-put box key value))

(defun xmp-isobmff-udta-meta-put (box key value)
  (when value
    (when (stringp value)
      (setq value (string-trim value)))
    (unless (and (stringp value) (string-empty-p value))
      (let* ((root-box (xmp-isobmff-box-root box))
             (cell (xmp-isobmff-box-get-property-cell-create root-box :udta-meta)))
        (setf (alist-get key (cadr cell)) value)))))

;; ;; moov/meta?
;; (defun xmp-isobmff-read-udta-id32 (reader box) ;; ID3v2
;;   (xmp-isobmff-read-full-box reader box)
;;   (when (eq (plist-get box :version) 0)
;;     (xmp-file-reader-u16be reader) ;; Language
;;     (xmp-isobmff-box-set-property
;;      box
;;      :id3
;;      (xmp-file-reader-read-bytes
;;       reader
;;       (xmp-isobmff-box-remaining-size box reader)))
;;     nil))

;;;;;; iTunes

(defconst xmp-isobmff-mdir-ilst-tag-info-list
  ;; https://exiftool.org/TagNames/QuickTime.html#ItemList
  `((,(xmp-isobmff-fourcc "©nam") string title)
    (,(xmp-isobmff-fourcc "©ART") string artist)
    ;; (,(xmp-isobmff-fourcc "aART") string) ;; AlbumArtist
    (,(xmp-isobmff-fourcc "©alb") string album)
    (,(xmp-isobmff-fourcc "©gen") string genre)
    (,(xmp-isobmff-fourcc "trkn") rational track-number) ;; 0 track-number total-track-number 0?(disc-number?) (8-bytes)
    (,(xmp-isobmff-fourcc "disk") rational disc-number) ;; 0 disc-number total-disc-number (6-bytes)
    ;; (,(xmp-isobmff-fourcc "cpil") u8) ;; Compilation flags=#x15 u8(0 or 1)
    ;; (,(xmp-isobmff-fourcc "pgap") u8) ;; PlayGap flags=#x15 u8(0 or 1)
    (,(xmp-isobmff-fourcc "©day") string date) ;; ContentCreateDate
    ;; (,(xmp-isobmff-fourcc "apID") string) ;; AppleStoreAcount
    ;; (,(xmp-isobmff-fourcc "ownr") string) ;; Owner
    (,(xmp-isobmff-fourcc "cprt") string copyright) ;; Copyright
    ;; (,(xmp-isobmff-fourcc "cnID") u32) ;; AppleStoreCatalogID flag=#x15 u32?
    ;; (,(xmp-isobmff-fourcc "rtng") u8) ;; Rating flag=#x15 u8
    ;; (,(xmp-isobmff-fourcc "atID") u32) ;; ArtistID flag=#x15 u32
    ;; (,(xmp-isobmff-fourcc "plID") u64) ;; AlbumID flag=#x15 u64?
    ;; (,(xmp-isobmff-fourcc "geID") u32) ;; GenreID flag=#x15 u32
    ;; (,(xmp-isobmff-fourcc "sfID") u32) ;; AppleStoreCountry flag=#x15 u32
    ;; (,(xmp-isobmff-fourcc "stik") u8) ;; MediaType flag=#x15 u8
    ;; (,(xmp-isobmff-fourcc "purd") string) ;; PurchaseDate
    ;; (,(xmp-isobmff-fourcc "sonm") string) ;; SortName
    ;; (,(xmp-isobmff-fourcc "soal") string) ;; SortAlbum
    ;; (,(xmp-isobmff-fourcc "soar") string) ;; SortArtist
    ;; (,(xmp-isobmff-fourcc "xid ") string)) ;; ISRC
    ))

(defconst xmp-isobmff-mdir-ilst-handler-list
  (cl-loop for (type tag-type) in xmp-isobmff-mdir-ilst-tag-info-list
           collect (cons
                    type
                    (intern
                     (format "xmp-isobmff-read-mdir-ilst-tag-%s" tag-type)))))

(defun xmp-isobmff-read-mdir-ilst (reader box)
  (when-let* ((tags (xmp-isobmff-read-box-children
                     reader
                     box
                     xmp-isobmff-mdir-ilst-handler-list)))
    (xmp-isobmff-box-set-property (xmp-isobmff-box-root box) :mdir-meta tags)
    nil))

(defun xmp-isobmff-read-mdir-ilst--tag-cons (box values)
  (when values
    (when-let* ((value (car values))
                (type (plist-get box :type))
                (info (assoc type xmp-isobmff-mdir-ilst-tag-info-list))
                (symbol (nth 2 info)))
      (when (stringp value)
        (setq value (string-trim value)))
      (unless (and (stringp value) (string-empty-p value))
        (cons symbol value)))))

(defun xmp-isobmff-read-mdir-ilst-tag-string (reader box)
  (xmp-isobmff-read-mdir-ilst--tag-cons
   box
   (xmp-isobmff-read-box-children
    reader
    box
    `((,(xmp-isobmff-fourcc "data") . xmp-isobmff-read-mdir-ilst-tag-string-data)))))

(defun xmp-isobmff-read-mdir-ilst-tag-string-data (reader box)
  (xmp-isobmff-read-full-box reader box)
  (xmp-file-reader-skip reader 4) ;; Reserved?
  (decode-coding-string
   (xmp-file-reader-read-bytes reader
                               (xmp-isobmff-box-remaining-size box reader))
   'utf-8))

(defun xmp-isobmff-read-mdir-ilst-tag-rational (reader box)
  (xmp-isobmff-read-mdir-ilst--tag-cons
   box
   (xmp-isobmff-read-box-children
    reader
    box
    `((,(xmp-isobmff-fourcc "data") . xmp-isobmff-read-mdir-ilst-tag-rational-data)))))

(defun xmp-isobmff-read-mdir-ilst-tag-rational-data (reader box)
  (xmp-isobmff-read-full-box reader box)
  (xmp-file-reader-skip reader 4) ;; Reserved?
  ;; trkn 0 track-number total 0?(disc-number?)
  ;; disk 0 disc-number total
  (xmp-file-reader-skip reader 2) ;; 0?
  (let ((numer (xmp-file-reader-u16be reader))
        (denom (xmp-file-reader-u16be reader)))
    (xmp-isobmff-box-skip-to-end box reader) ;; SKip 0 if exists

    (when (/= numer 0)
      (if (/= denom 0)
          (format "%d/%d" numer denom)
        (format "%d" numer)))))

;; (defun xmp-isobmff-read-mdir-ilst-tag-u32 (reader box)
;;   (xmp-isobmff-read-mdir-ilst--tag-cons
;;    box
;;    (xmp-isobmff-read-box-children
;;     reader
;;     box
;;     `((,(xmp-isobmff-fourcc "data") . xmp-isobmff-read-mdir-ilst-tag-u32-data)))))

;; (defun xmp-isobmff-read-mdir-ilst-tag-u32-data (reader box)
;;   (xmp-isobmff-read-full-box reader box)
;;   (xmp-file-reader-skip reader 4) ;; Reserved?
;;   (xmp-file-reader-u32be reader))

;;;;;; XMP Box

(defun xmp-isobmff-read-xmp (reader box)
  ;; xmp-begin=>|<?xpacket begin= ...> ... <?xpacket end= ...>|<=xmp-end
  (let ((xmp-begin (xmp-file-reader-current-offset reader))
        (xmp-end (xmp-isobmff-box-end-offset box)))
    (when xmp-isobmff-verbose
      (message "XMP box %x ~ %x" xmp-begin xmp-end))
    (xmp-isobmff-box-set-property
     (xmp-isobmff-box-root box)
     :xmp
     (list :begin xmp-begin
           :end xmp-end
           :bytes (xmp-file-reader-read-bytes reader
                                              (- xmp-end xmp-begin))))))

;;;;;; EXIF Box

(defun xmp-isobmff-read-exif (reader box)
  ;; exif-begin=>|II or MM .... |<=exif-end
  (let ((exif-begin (xmp-file-reader-current-offset reader))
        (exif-end (xmp-isobmff-box-end-offset box)))
    (when xmp-isobmff-verbose
      (message "EXIF box %x ~ %x" exif-begin exif-end))
    (xmp-isobmff-box-set-property
     (xmp-isobmff-box-root box)
     :exif
     (list :begin exif-begin
           :end exif-end
           :bytes (xmp-file-reader-read-bytes reader
                                              (- exif-end exif-begin))))))

;;;;; Read File as XML

;;;###autoload
(defun xmp-isobmff-read-xmp-dom (file)
  (when-let* ((box (xmp-isobmff-read-metadata-from-file file)))
    (let ((xmp (plist-get box :xmp))
          (exif (plist-get box :exif))
          (udta-meta (plist-get box :udta-meta))
          (mdir-meta (plist-get box :mdir-meta))
          dom)

      (when xmp
        (setq dom (xmp-xml-parse-string
                   (decode-coding-string (plist-get xmp :bytes) 'utf-8))))

      (when exif
        (when-let* ((prop-elem-list
                     (xmp-exif-read-exif-as-xmp-property-elements-from-bytes
                      (plist-get exif :bytes))))
          (setq dom
                (xmp-merge-xml-dom-and-property-elements dom prop-elem-list))))

      (when udta-meta
        (setq dom (xmp-dm-convert-property-alist-to-xmp-dom udta-meta dom t)))

      (when mdir-meta
        (setq dom (xmp-dm-convert-property-alist-to-xmp-dom mdir-meta dom t)))

      dom)))


(provide 'xmp-file-dynamic-media)
;;; xmp-file-dynamic-media.el ends here
