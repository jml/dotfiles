;; Environment
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package direnv)

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-M-c" . mc/edit-lines)))

;; Diff highlighting in buffer
(use-package diff-hl
  :config (global-diff-hl-mode))

;; More efficient way of showing line numbers
(use-package nlinum
  :config
  (global-nlinum-mode)
  (setq nlinum-highlight-current-line t))

;; Helm

(use-package helm
  :after diminish
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list))
  :config (helm-mode 1))

;; Projectile

(use-package helm-rg)
(use-package helm-projectile)

(use-package projectile
  :after helm-projectile
  :bind ("C-c h" . helm-projectile)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; If we don't have this, we get tonnes of crazy arguments on the command-line.
  (setq helm-projectile-ignore-strategy 'search-tool)
  (projectile-global-mode)
  (helm-projectile-on)
  (setq projectile-use-git-grep 1))

(use-package popwin
  :config
  (popwin-mode 1))

;; Magit

(use-package transient)

;; Editing

(use-package paredit)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Snippets
(use-package yasnippet
  :after diminish
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package yasnippet-snippets)

;; Go

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :bind (:map go-mode-map ([remap xref-find-definitions] . godef-jump))
  :custom
  (godef-command "/Users/jml/go/bin/godef")
  (gofmt-args '("-s" "-w")))

(use-package flycheck
  :custom
  (flycheck-flake8rc ".flake8")
  (flycheck-go-build-install-deps t)
  (flycheck-go-golint-executable "/Users/jml/go/bin/golint")
  (flycheck-hlint-language-extensions '("TypeApplications")))

(use-package flycheck-gometalinter
  :after flycheck
  :config
  (flycheck-gometalinter-setup)
  (setq flycheck-gometalinter-vendor t)
  (setq flycheck-gometalinter-fast t))

(use-package go-impl)
(use-package go-projectile)
(use-package go-snippets)

(use-package rst
  :hook (rst-mode . variable-pitch-mode)

  :custom-face
  (rst-level-1 ((t :height 2.0)))
  (rst-level-2 ((t :height 1.5)))
  (rst-level-3 ((t :height 1.3)))
  (rst-level-4 ((t :height 1.2)))
  (rst-level-5 ((t :height 1.1))))

;; Make my custom utilities avaliable
(use-package jml-utils
  :straight nil)

;; Make my org-roam extensions available
(use-package jml-org-roam-utils
  :bind-keymap ("C-c n j" . jml/org-roam-utils-map)
  :straight nil
  :config
  (add-to-list 'popwin:special-display-config
               '("*org-roam dailies summary*" :position left :width 80)))

;; Rust

(use-package cargo)
(use-package rust-mode)

;; Terraform

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

;; Docker
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

;; vterm
;;
;; https://github.com/akermu/emacs-libvterm
;;
;; Make sure you do https://github.com/akermu/emacs-libvterm#shell-side-configuration also
(use-package vterm
  :config
  (setq vterm-always-compile-module t))

