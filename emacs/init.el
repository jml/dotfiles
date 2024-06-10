;; Customize
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Load manually-managed third-party plugins.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))
(require 'load-directory)
(require 'jml-utils)

;; Configuration that's appropriate in all the places I use Emacs.
(load-directory (expand-file-name "~/.emacs.d/config.d"))

;; Configuration that's only appropriate in this installation.
(load-directory (expand-file-name "~/.emacs.d/local.d"))

