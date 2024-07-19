;;; Package -- Summary
;;; Commentary:
;;;
;;; General purpose Emacs functions, centered around filesystem operations on open buffers.
;;;
;;; Code:

(provide 'jml-utils)

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


(defun jml/org-roam-inbox ()
  (interactive)
  (dired (concat org-roam-directory "/Inbox")))


(defun jml/org-roam-archive-buffer-file ()
  "Move the current file to the Archive."
  (interactive)

  (let*
      ((archive-dir (concat org-roam-directory "/Archive"))
       (filename (buffer-file-name)))
    (jml/org-roam-move-buffer-file archive-dir)
    (message "Moved %s to %s" filename archive-dir)
    (kill-buffer)))


;; Experimental code for getting the whole week of daily captures
;; (require 'org-roam-dailies)

;; (defun jml/org-roam-dailies-last-calendar-week ()
;;   "Gather org-roam daily captures for the last calendar week and insert them into a new buffer."
;;   (interactive)
;;   (let* ((org-roam-directory (expand-file-name org-roam-directory))
;;          (daily-directory (expand-file-name "daily" org-roam-directory))
;;          (current-date (current-time))
;;          (last-sunday (time-subtract current-date (days-to-time (mod (1+ (calendar-day-of-week current-date)) 7))))
;;          (last-monday (time-subtract last-sunday (days-to-time 6)))
;;          (date-list (cl-loop for days from 0 to 6
;;                              collect (format-time-string "%Y-%m-%d" (time-add last-monday (days-to-time days)))))
;;          (file-list (mapcar (lambda (date)
;;                               (expand-file-name (format "%s.org" date) daily-directory))
;;                             date-list))
;;          (content ""))
;;     (dolist (file file-list)
;;       (when (file-exists-p file)
;;         (setq content (concat content "\n" (with-temp-buffer
;;                                              (insert-file-contents file)
;;                                              (buffer-string))))))
;;     (with-current-buffer (get-buffer-create "*Org-roam Weekly Summary*")
;;       (erase-buffer)
;;       (insert content)
;;       (org-mode)
;;       (goto-char (point-min))
;;       (display-buffer (current-buffer)))))

;; ;; Bind the function to a key for convenience
;; (global-set-key (kbd "C-c n w") 'my/org-roam-dailies-last-calendar-week)



;;; jml-utils.el ends here
