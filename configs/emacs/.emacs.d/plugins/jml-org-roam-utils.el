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

(require 'jml-utils)
(require 'org-roam)
(require 'org-roam-dailies)
(require 'seq)

(defvar jml/org-roam-archive-dir "Archive")

(defvar jml/org-roam-inbox-dir "Inbox")

(defvar jml/org-roam-weekly-summary-start-day-of-week 6)


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
  (dired (expand-file-name jml/org-roam-inbox-dir org-roam-directory)))


(defun jml/org-roam-archive-buffer-file ()
  "Move the current file to the Archive."
  (interactive)

  (let*
      ((archive-dir (expand-file-name jml/org-roam-archive-dir org-roam-directory))
       (filename (buffer-file-name)))
    (jml/org-roam-move-buffer-file archive-dir)
    (message "Moved %s to %s" filename archive-dir)
    (kill-buffer)))


(defun jml/generate-org-roam-dailies-summary (start-date end-date)
  "Gather org-roam daily captures between START-DATE and END-DATE and insert them into a new, temporary buffer."
  (let* ((daily-directory (expand-file-name org-roam-dailies-directory org-roam-directory))
         (absolute-start-date (calendar-absolute-from-gregorian start-date))
         (absolute-end-date (calendar-absolute-from-gregorian end-date))
         (date-list (cl-loop for day from absolute-start-date to absolute-end-date
                             collect (jml/format-date-iso (calendar-gregorian-from-absolute day))))
         (file-list (mapcar (lambda (date)
                              (expand-file-name (format "%s.org" date) daily-directory))
                            date-list))
         (all-daily-notes (with-temp-buffer
                            (dolist (file file-list)
                              (when (file-exists-p file)
                                (insert-file-contents file)
                                (end-of-buffer)
                                (insert "\n")
                                (end-of-buffer)))
                            (buffer-string))))

    (with-current-buffer (get-buffer-create "*org-roam dailies summary*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert all-daily-notes)
      (org-mode)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))


(defun jml/parse-org-time-string (time-string)
  "Parse TIME-STRING from org-read-date into a calendar date of the form (MONTH DAY YEAR)."
  (let ((parsed-string (org-parse-time-string time-string)))
    (cl-destructuring-bind (_ _ _ day month year _ _ _) parsed-string
      (list month day year))))


(defun jml/org-roam-dailies-summary (start-date end-date)
  "Gather org-roam daily captures between START-DATE and END-DATE and insert them into a new, temporary buffer."
  (interactive
   (list (org-read-date nil nil nil "Start date"
                        (jml/time-from-gregorian
                         (jml/gregorian-last-day-before
                          (calendar-current-date)
                          jml/org-roam-weekly-summary-start-day-of-week)))
         (org-read-date nil nil nil "End date")))
  (let ((calendar-start-date (jml/parse-org-time-string start-date))
        (calendar-end-date (jml/parse-org-time-string end-date)))
    (jml/generate-org-roam-dailies-summary calendar-start-date calendar-end-date)))


(defun jml/org-roam-open-random-inbox-note ()
  "Open a random note from the Org-roam Inbox directory."
  (interactive)
  (let* ((inbox-dir (expand-file-name jml/org-roam-inbox-dir org-roam-directory))
         (files (directory-files-recursively inbox-dir "\\.org$")))
    (if (null files)
        (message "No notes found in Inbox directory.")
      (find-file (seq-random-elt files)))))


(defvar jml/org-roam-utils-map (make-sparse-keymap)
  "Keymap for `jml-org-roam-utils`.")

(define-prefix-command 'jml/org-roam-utils-map)

(define-key jml/org-roam-utils-map (kbd "i") #'jml/org-roam-go-to-inbox)
(define-key jml/org-roam-utils-map (kbd "s") #'jml/org-roam-dailies-summary)
(define-key jml/org-roam-utils-map (kbd ".") #'jml/org-roam-open-random-inbox-note)
(define-key jml/org-roam-utils-map (kbd "y") #'jml/org-roam-archive-buffer-file)


(provide 'jml-org-roam-utils)

;;; jml-org-roam-utils.el ends here
