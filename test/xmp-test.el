;;; test.el ---                                      -*- lexical-binding: t; -*-

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

(defun xmp--test-load (filename)
  (load (expand-file-name filename (or load-file-name default-directory))))

(defun xmp--test ()
  (interactive)
  (xmp--test-load "xmp-test-value-types.el")
  (xmp--test-load "xmp-test-syntax-property-elements.el")
  (ert "^xmp--test"))

;;; test.el ends here
