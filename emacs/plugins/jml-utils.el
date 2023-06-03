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

;;; jml-utils.el ends here

