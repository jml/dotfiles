;;; 22-python.el --- Python development configuration

;;; Commentary:
;; Configuration for Python development

;;; Code:

;; Python

;; Basic Python mode
(use-package python)

;; pyenv integration
(use-package pyenv-mode
  :init (add-to-list 'exec-path "~/.pyenv/shims")
  :config (pyenv-mode)
  :hook (python-mode . pyenv-mode))

(use-package lsp-pyright
  :hook (python-mode . lsp-deferred)
  :custom
  (lsp-pyright-multi-root nil) ;; for performance
  (lsp-idle-delay 0.5) ;; for performance
  (lsp-enable-file-watchers nil) ;; for performance
  (lsp-pyright-langserver-command "pyright")) ;; Use pyright as LSP server

