;; Customize
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Load manually-managed third-party plugins.
(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))
(require 'load-directory)

;; Emacs seems to want a temp dir
(let ((temp-dir (expand-file-name "temp" user-emacs-directory)))
  (unless (file-directory-p temp-dir)
    (make-directory temp-dir)))

;; Configuration that's only appropriate in this installation.
(let ((local-dir (expand-file-name "local.d" user-emacs-directory)))
  (when (file-directory-p local-dir)
    (load-directory local-dir)))

;; Configuration that's appropriate in all the places I use Emacs.
(load-directory (expand-file-name "config.d" user-emacs-directory))
