;;; 22-python.el --- Python development configuration

;;; Commentary:
;; Configuration for Python development

;;; Code:

;; Python

;; Okay kiddywinks.
;; pyright doesn't support attrs: https://github.com/microsoft/pyright/issues/146
;; Other Python language servers give a confusing mess of virtualenv hell.
;;
;; I'm going to start building things myself, one at a time.
;; Here's what I need:
;;
;; - syntax highlighting
;; - flycheck/make running flake8 & mypy with the config for the project
;; - go to definition (Jedi?)
;;
;; First thing I'll try is "naked" Python mode, and then see what we're missing.
;;
;; Eldoc is running and I get prompted about LSP but nothing actually runs.
;; No inline code checks and no jump to definition.
;; Python interpreter has no idea about virtualenvs.
;;
;; Let's get virtualenvs working first.
;; I'm pretty committed to pyenv on the command line so maybe we can start with that.

;; Next up is flycheck
;; I don't think there's a compelling reason to use flymake instead.
;; For almost all projects, I'll want flake8 & mypy.
;; I need to make sure:
;;
;; - flycheck is enabled by default
;;
;; All we need to do is enable flycheck mode globally.

;; Getting go to definition is a bit trickier.
;; I tried jedi-lsp, but it clashes with the previous flycheck work and I don't know how to get lsp to stop overriding flycheck.
;; I have installed `jedi` and run `jedi:install-server`.
;; It seems to do the trick.

(use-package python)

(use-package jedi
  :hook (python-mode . jedi:setup)
  :config
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t))

(provide '22-python)
;;; 22-python.el ends here
