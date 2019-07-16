(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t)

(use-package flycheck-haskell
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-haskell-setup))

(use-package flycheck-gometalinter
  :ensure t
  :after flycheck
  :config
  (flycheck-gometalinter-setup)
  (setq flycheck-gometalinter-vendor t)
  (setq flycheck-gometalinter-fast t))

(use-package go-mode
  :ensure t
  :hook (before-save . gofmt-before-save)
  :bind (:map go-mode-map ([remap xref-find-definitions] . godef-jump)))

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . turn-on-haskell-indentation)
         (haskell-mode . turn-on-haskell-doc)
         (haskell-mode . turn-on-haskell-decl-scan)))

(use-package intero
  :ensure t
  :after haskell-mode
  :hook (haskell-mode . intero-mode)
  :config (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(use-package helm
  :config
  (helm-mode 1))

(use-package helm-config
  :after helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package lsp-mode
  :hook (prog-mode . lsp)
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package dap-mode)

(use-package magit
  :bind (("\C-c g g" . magit-status)
         ("<f12>" . magit-status))
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package markdown-mode
  :mode (("\\.text" . markdown-mode)
         ("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :config
  ;; Make markdown look prettier
  (set-face-attribute 'markdown-header-face-1 nil :inherit 'markdown-header-face :height 2.0)
  (set-face-attribute 'markdown-header-face-2 nil :inherit 'markdown-header-face :height 1.5)
  (set-face-attribute 'markdown-header-face-3 nil :inherit 'markdown-header-face :height 1.3)
  (set-face-attribute 'markdown-header-face-4 nil :inherit 'markdown-header-face :height 1.2)
  (set-face-attribute 'markdown-header-face-5 nil :inherit 'markdown-header-face :height 1.1))

(use-package org
  :bind ("\C-c a" . org-agenda)
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
            (set-face-attribute 'org-headline-done nil :foreground "#859900")
            (set-face-attribute 'org-level-1 nil :foreground "#ffffff" :height 1.5)
            (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :foreground "#ffffff")
            (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :foreground "#ffffff")))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package popwin
  :config (progn
            (popwin-mode 1)
            (global-set-key (kbd "C-z") popwin:keymap)))

;; TODO: Express that this depends on helm-projectile?
(use-package projectile
  :ensure t
  :bind ("C-c h" . helm-projectile)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (helm-projectile-on)
  (setq projectile-use-git-grep 1))

(use-package helm-ag
  :ensure t)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-M-c" . mc/edit-lines)))

(use-package python
  :ensure t)

(use-package blacken
  :after python
  :hook (python-mode blacken-mode))

(use-package isortify
  :after python
  :hook (python-mode isortify-mode))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))
