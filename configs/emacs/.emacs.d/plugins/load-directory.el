(provide 'load-directory)

(defun load-directory (dir)
  "Load all the .el files in dir."
  (mapcar #'(lambda (x) (load-file x))
          (directory-files dir t "\\.el$")))
