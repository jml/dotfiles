;;; 30-development.el --- General Development Tools

;;; Commentary:
;; Language-agnostic development utilities including linting, syntax checking,
;; snippets, and editing enhancements. These tools work across multiple
;; programming languages and improve the general development experience.

;;; Code:

;; Flycheck - syntax checking framework
(use-package flycheck
  :custom
  (flycheck-flake8rc ".flake8")
  (flycheck-go-build-install-deps t)
  (flycheck-go-golint-executable "/Users/jml/go/bin/golint")
  (flycheck-hlint-language-extensions '("TypeApplications")))

;; Transient interface library (used by Magit and others)
(use-package transient)

;; Structured editing for Lisp-like languages
(use-package paredit)

;; Expand selection by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Yasnippet - template system
(use-package yasnippet
  :after diminish
  :diminish yas-minor-mode
  :config (yas-global-mode))

;; Collection of snippets for various languages
(use-package yasnippet-snippets)

;;; 30-development.el ends here