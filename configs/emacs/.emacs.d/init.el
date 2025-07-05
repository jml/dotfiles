;; Don't use custom.el
(setq custom-file "/dev/null")

;; Load manually-managed third-party plugins.
(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))
(require 'load-directory)

;; Configuration that's only appropriate in this installation.
(let ((local-dir (expand-file-name "local.d" user-emacs-directory)))
  (when (file-directory-p local-dir)
    (load-directory local-dir)))

;; Configuration that's appropriate in all the places I use Emacs.
(load-directory (expand-file-name "config.d" user-emacs-directory))
