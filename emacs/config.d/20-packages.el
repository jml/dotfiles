;; Automatically install packages not on my system
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Automatically upgrade packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Theme

;; Other themes I have loved:
;; - monokai
;; - noctilux
;; - spacegray
(use-package dracula-theme
  :config
  (load-theme 'dracula))

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
  :after diminish
  :diminish helm-mode
  :config
  (helm-mode 1))

(use-package helm-config
  :ensure nil
  :after helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package helm-rg)
(use-package helm-git-grep)

;; Projectile

(use-package helm-projectile)

(use-package projectile
  :after helm-projectile
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
  :after diminish
  :diminish auto-revert-mode
  :bind (("\C-c g g" . magit-status)
         ("<f12>" . magit-status))
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

;; git-link
;;
;; Interactive Emacs functions that create URLs for files and commits in
;; GitHub repositories.
(use-package git-link)

;; Smart Mode Line
;;
;; A better mode line
(use-package diminish)

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

;; Flycheck

(use-package flycheck
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :config (flycheck-add-next-checker 'python-flake8 'python-pylint))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

;; LSP

(use-package lsp-mode
  :hook (rust-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :after diminish
  :diminish eldoc-mode
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

(use-package company-box
  :after diminish
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

;; TODO: Investigate DAP mode
(use-package dap-mode)

;; Snippets
(use-package yasnippet
  :after diminish
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package yasnippet-snippets)

;; Dash

;; TODO: Register a keyboard binding for dash-at-point
(use-package dash-at-point)

;; LANGUAGES

;; Python
(use-package elpy
  :after flycheck
  :init
  (elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

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
  :custom-face
  (markdown-header-face-1 ((t (:inherit 'markdown-header-face :height 2.0))))
  (markdown-header-face-2 ((t (:inherit 'markdown-header-face :height 1.5))))
  (markdown-header-face-3 ((t (:inherit 'markdown-header-face :height 1.3))))
  (markdown-header-face-4 ((t (:inherit 'markdown-header-face :height 1.2))))
  (markdown-header-face-5 ((t (:inherit 'markdown-header-face :height 1.1)))))

;; Python

(use-package python)

(use-package pyenv-mode)
(use-package python-docstring)
(use-package pipenv)

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
