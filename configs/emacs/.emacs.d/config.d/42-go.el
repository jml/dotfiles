;;; 42-go.el --- Go Development Configuration

;;; Commentary:
;; Configuration for Go development including:
;; - Basic Go mode setup with formatting
;; - LSP integration for Go development
;; - Go-specific tools and utilities
;; - Project and snippet support

;;; Code:

;; Basic Go mode
(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :bind (:map go-mode-map ([remap xref-find-definitions] . godef-jump))
  :custom
  (godef-command "/Users/jml/go/bin/godef")
  (gofmt-args '("-s" "-w")))

;; LSP integration for Go
(use-package lsp-mode
  :hook (go-mode . lsp)
  :commands lsp)

;; Go implementation generator
(use-package go-impl)

;; Projectile integration for Go projects
(use-package go-projectile)

;; Go snippets
(use-package go-snippets)

;;; 42-go.el ends here
