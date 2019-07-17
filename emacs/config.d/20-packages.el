;; Automatically install packages not on my system
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Automatically upgrade packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-M-c" . mc/edit-lines)))

(use-package diff-hl
  :config (global-diff-hl-mode))

;; Helm

(use-package helm
  :config
  (helm-mode 1))

(use-package helm-config
  :ensure nil
  :after helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package helm-ag)

;; Projectile

;; TODO: Express that this depends on helm-projectile?
(use-package projectile
  :bind ("C-c h" . helm-projectile)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (helm-projectile-on)
  (setq projectile-use-git-grep 1))

;; Magit

(use-package magit
  :bind (("\C-c g g" . magit-status)
         ("<f12>" . magit-status))
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

;; Flycheck

(use-package flycheck)

;; LSP

(use-package lsp-mode
  :hook (prog-mode . lsp)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

;; TODO: Investigate DAP mode
(use-package dap-mode)

;; Snippets
(use-package yasnippet
  :config (yas-global-mode))

(use-package yasnippet-snippets)

;; LANGUAGES

;; Go

(use-package flycheck-gometalinter
  :after flycheck
  :config
  (flycheck-gometalinter-setup)
  (setq flycheck-gometalinter-vendor t)
  (setq flycheck-gometalinter-fast t))

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :bind (:map go-mode-map ([remap xref-find-definitions] . godef-jump)))

;; Haskell

(use-package haskell-mode
  :hook ((haskell-mode . turn-on-haskell-indentation)
         (haskell-mode . turn-on-haskell-doc)
         (haskell-mode . turn-on-haskell-decl-scan)))

(use-package flycheck-haskell
  :after flycheck
  :hook (flycheck-mode . flycheck-haskell-setup))

(use-package intero
  :after haskell-mode
  :hook (haskell-mode . intero-mode)
  :config (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

;; Markdown
(use-package markdown-mode
  :mode (("\\.text" . markdown-mode)
         ("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :config
  ;; TODO: Use custom-face for this
  ;; Make markdown look prettier
  (set-face-attribute 'markdown-header-face-1 nil :inherit 'markdown-header-face :height 2.0)
  (set-face-attribute 'markdown-header-face-2 nil :inherit 'markdown-header-face :height 1.5)
  (set-face-attribute 'markdown-header-face-3 nil :inherit 'markdown-header-face :height 1.3)
  (set-face-attribute 'markdown-header-face-4 nil :inherit 'markdown-header-face :height 1.2)
  (set-face-attribute 'markdown-header-face-5 nil :inherit 'markdown-header-face :height 1.1))

(use-package org
  :bind ("\C-c a" . org-agenda)
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (set-face-attribute 'org-headline-done nil :foreground "#859900")
  (set-face-attribute 'org-level-1 nil :foreground "#ffffff" :height 1.5)
  (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :foreground "#ffffff")
  (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :foreground "#ffffff"))

;; Python

(use-package python)

(use-package blacken
  :after python
  :hook (python-mode blacken-mode))

(use-package isortify
  :after python
  :hook (python-mode isortify-mode))

;; Terraform

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))
