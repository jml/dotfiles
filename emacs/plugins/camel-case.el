;;; jml-utils.el --- Personal editing utilities

;; Copyright (C) 2009  Jonathan Lange

;; Author: Jonathan Lange <jml@mumak.net>
;; Keywords: local, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
   Default for SEP is a hyphen \"-\".

   If third argument START is non-nil, convert words after that
   index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "_")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

(defun uccw (&optional n)
  "Convert a camel-case word to an underscore-separated word."
  (interactive "p")
  (let* ((beg (point))
         (end (save-excursion (forward-word n) (point)))
         (string (un-camelcase-string (buffer-substring beg end))))
    (delete-region beg end) (insert string)))

(defun unstudlify-region (beg end)
  "Convert \"studlyCaps\" in the region to underscore-separated form."
  (interactive "r")
  (let ((case-fold-search nil) ch)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "[[:lower:][:digit:]][[:upper:]]" nil t)
	(setq ch (downcase (char-before)))
	(delete-char -1)
	(insert ?_ ch)))))

(defalias 'un-camel-case-region 'unstudlify-region)


(provide 'camel-case)
;;; camel-case ends here
