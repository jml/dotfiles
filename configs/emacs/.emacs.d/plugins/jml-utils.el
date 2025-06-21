;;; Package -- Summary
;;; Commentary:
;;;
;;; General purpose Emacs functions, centered around filesystem operations on open buffers.
;;;
;;; Code:

(require 'calendar)

(defun jml/delete-file-and-buffer ()
  "Kill the current buffer and delete the file it is visiting.

If the file is in version control, delete it using 'vc-delete-file'.
Otherwise, prompt the user, and then delete the file directly.

We don't prompt the user when there is version control,
because that gives them an easy way to undo their decision."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (if (y-or-n-p (concat "Do you really want to delete file " filename "?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s" filename)
              (kill-buffer)))))))


(defun jml/move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR.

If START-DIRECTORY is supplied, use that as the directory to start with."
  (interactive
   (list
    (read-directory-name
     "Target directory: "
     nil nil t nil)))
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


(defun jml/format-date-iso (date)
  "Format DATE (a list of (MONTH DAY YEAR)) into a string 'YYYY-MM-DD'."
  (cl-destructuring-bind (month day year) date
    (format "%04d-%02d-%02d" year month day)))


(defun jml/absolute-last-day-before (date daynum)
  "Return the last occurrence of DAYNUM strictly before DATE.

DATE is an absolute date from `calendar`.
DAYNUM is the day of the week."
  (let ((last-day (calendar-dayname-on-or-before daynum date)))
    (if (equal last-day date)
        (- last-day 7)
      last-day)))


(defun jml/gregorian-last-day-before (date daynum)
  "Return the last occurrence of DAYNUM strictly before DATE.

Like `jml/absolute-last-day-before`, except this accepts and returns a Gregorian
date in the form (MONTH DAY YEAR)."
  (calendar-gregorian-from-absolute
   (jml/absolute-last-day-before
    (calendar-absolute-from-gregorian date)
    daynum)))


(defun jml/time-from-gregorian (date)
  (cl-destructuring-bind (month day year) date
    (encode-time (list 0 0 0 day month year 0 0 0))))


(provide 'jml-utils)

;;; jml-utils.el ends here
