;;; git-todo -- Tools for todos from git
;;;
;;; Commentary:

;;; Code:



(defmacro at-git-root (dirname &rest code)
  "At the root of the checkout which DIRNAME is in, run CODE."
  `(let ((default-directory (locate-dominating-file (expand-file-name ,dirname) ".git"))) ,@code))

(defun git-todo ()
  "Find all TODOs in current branch."
  (interactive)
  (at-git-root "." (compile "git diff master | /usr/local/bin/diff-todo" t)))

(defun git-all-todos ()
  "Find all TODOs in repository."
  (interactive)
  (at-git-root "." (compile "git ls-files | xargs /usr/local/bin/all-todos")))

(provide 'git-todo)

;;; git-todo.el ends here
