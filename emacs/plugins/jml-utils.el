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


;;; jml-utils.el ends here
