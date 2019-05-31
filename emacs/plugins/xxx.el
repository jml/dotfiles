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

;;; Code:

(defun insert-date ()
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))


(defun xxx ()
  "Insert a XXX comment."
  (interactive)
  (insert (format "# XXX: jml %s: " (format-time-string "%Y-%m-%d"))))


(provide 'xxx)
;;; xxx.el ends here
