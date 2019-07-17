;; Automatically install packages not on my system
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Automatically upgrade packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Environment

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package direnv)
(use-package projectile-direnv
  :after (direnv projectile))

;; Package management

(use-package paradox)

;; Multiple cursors

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-M-c" . mc/edit-lines)))

;; Diff highlighting in buffer

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
(use-package helm-git-grep)

;; Projectile

;; TODO: Express that this depends on helm-projectile?
(use-package projectile
  :bind ("C-c h" . helm-projectile)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (helm-projectile-on)
  (setq projectile-use-git-grep 1))

(use-package popwin
  :config
  (popwin-mode 1))

;; Magit

(use-package magit
  :bind (("\C-c g g" . magit-status)
         ("<f12>" . magit-status))
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

;; Smart Mode Line
;;
;; A better mode line
(use-package smart-mode-line-atom-one-dark-theme)

(use-package smart-mode-line
  :after smart-mode-line-atom-one-dark-theme
  :config
  (setq sml/theme 'atom-one-dark)
  (sml/setup))

;; Editing

(use-package paredit)

(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-a" . sp-backward-down-sexp)
        ("C-S-d" . sp-beginning-of-sexp)
        ("C-S-a" . sp-end-of-sexp)
        ("C-M-e" . sp-up-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-t" . sp-transpose-sexp)
        ("C-M-n" . sp-forward-hybrid-sexp)
        ("C-M-p" . sp-backward-hybrid-sexp)
        ("C-M-k" . sp-kill-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("M-<delete>" . sp-unwrap-sexp)
        ("M-<backspace>" . sp-backward-unwrap-sexp)
        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-<left>" . sp-forward-barf-sexp)
        ("C-M-<left>" . sp-backward-slurp-sexp)
        ("C-M-<right>" . sp-backward-barf-sexp)
        ("M-D" . sp-splice-sexp)
        ("C-M-<delete>" . sp-splice-sexp-killing-forward)
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" . sp-splice-sexp-killing-around)
        ("C-]" . sp-select-next-thing-exchange)
        ("C-<left_bracket>" . sp-select-previous-thing)
        ("C-M-]" . sp-select-next-thing)
        ("M-F" . sp-forward-symbol)
        ("M-B" . sp-backward-symbol)
        ("C-\"" . sp-change-inner)
        ("M-i" . sp-change-enclosing)))

;; Flycheck

(use-package flycheck)
(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

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

;; Dash

;; TODO: Register a keyboard binding for dash-at-point
(use-package dash-at-point)

;; LANGUAGES

;; Go

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :bind (:map go-mode-map ([remap xref-find-definitions] . godef-jump)))

(use-package flycheck-gometalinter
  :after flycheck
  :config
  (flycheck-gometalinter-setup)
  (setq flycheck-gometalinter-vendor t)
  (setq flycheck-gometalinter-fast t))

(use-package go-impl)
(use-package go-projectile)
(use-package go-snippets)

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

(use-package hindent)
(use-package haskell-snippets)

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

(use-package pyenv-mode)
(use-package python-docstring)
(use-package pipenv)

(use-package blacken
  :after python
  :hook (python-mode blacken-mode))

(use-package isortify
  :after python
  :hook (python-mode isortify-mode))

(use-package flycheck-pyflakes)

;; Rust

(use-package cargo)
(use-package rust-mode)

;; Terraform

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

;; Docker

(use-package docker)
(use-package dockerfile-mode)

;; Other languages

(use-package graphviz-dot-mode)
(use-package groovy-mode)
(use-package protobuf-mode)
(use-package toml-mode)
(use-package web-mode)
(use-package yaml-mode)

;; Nix
(use-package nix-mode)
(use-package pretty-sha-path)
