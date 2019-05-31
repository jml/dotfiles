
;; Run 'code' at the root of the branch which dirname is in.
(defmacro bzr-tools-at-branch-root (dirname &rest code)
  `(let ((default-directory (locate-dominating-file (expand-file-name ,dirname) ".bzr"))) ,@code))


(defun bzr-tools-branch-todo ()
  (interactive)
  (bzr-tools-at-branch-root "."
   (compile "bzr todo" t)))


(provide 'bzr-tools)
