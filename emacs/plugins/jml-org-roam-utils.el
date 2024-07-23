;;; jml-org-roam-utils.el --- summary -*- lexical-binding: t -*-

;; Author: Jonathan Lange
;; Maintainer: Jonathan Lange
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; My own extensions to org-roam, catered to my specific use cases.

;;; Code:

(require 'org-roam)

(defvar jml/org-roam-archive-dir "Archive")

(defvar jml/org-roam-inbox-dir "Inbox")

(defun jml/org-roam-move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR.

If START-DIRECTORY is supplied, use that as the directory to start with."
  (interactive
   (list
    (read-directory-name
     "Target directory: "
     (or org-roam-directory nil) nil t nil)))
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (rename-file filename newname 1)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))


(defun jml/org-roam-go-to-inbox ()
  "Browse the inbox folder with Dired."
  (interactive)
  (dired (concat (file-name-as-directory org-roam-directory) jml/org-roam-inbox-dir)))


(defun jml/org-roam-archive-buffer-file ()
  "Move the current file to the Archive."
  (interactive)

  (let*
      ((archive-dir (concat (file-name-as-directory org-roam-directory) jml/org-roam-archive-dir))
       (filename (buffer-file-name)))
    (jml/org-roam-move-buffer-file archive-dir)
    (message "Moved %s to %s" filename archive-dir)
    (kill-buffer)))


(provide 'jml-org-roam-utils)

;;; jml-org-roam-utils.el ends here
