
(req-package flycheck
  :config
  (progn
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

(req-package flycheck-gometalinter
  :require (flycheck)
  :config
  (progn
    (flycheck-gometalinter-setup)
    (setq flycheck-gometalinter-vendor t)
    (setq flycheck-gometalinter-fast t)))

(req-package go-mode
  :config
  (progn (add-hook 'before-save-hook 'gofmt-before-save)
         (add-hook 'go-mode-hook (lambda () (local-set-key (kbd "M-.") #'godef-jump)))))

(req-package haskell-mode
  :require (haskell-process ghc-nix haskell-style intero)
  :config (progn
            (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)
            ;; intero-mode stupidly slow on Haskell scripts
            (add-hook 'haskell-mode-hook #'intero-mode)
;;            (add-hook 'haskell-mode-hook #'use-nix-ghc-in-flycheck)
;;            (global-intero-mode)
            (flycheck-add-next-checker 'intero
                                       '(warning . haskell-hlint))))


;; See also (add-hook 'haskell-mode-hook #'haskell-style) for switching on johanTibell style

;; TODO: hindent set up

(req-package helm-config
  :config (progn
            (helm-mode 1)
            (global-set-key (kbd "M-x") 'helm-M-x)
            (global-set-key (kbd "C-x C-f") 'helm-find-files)))

(req-package lsp-mode
  :hook (prog-mode . lsp)
  :commands lsp)

;; optionally
(req-package lsp-ui :commands lsp-ui-mode)
(req-package company-lsp :commands company-lsp)
(req-package helm-lsp :commands helm-lsp-workspace-symbol)
(req-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(req-package dap-mode)

(req-package magit
  :bind (("\C-c g g" . magit-status)
         ("<f12>" . magit-status))
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

(req-package markdown-mode
  :config (progn
            (setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
            (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
            (setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

            ;; Make markdown look prettier
            (set-face-attribute 'markdown-header-face-1 nil :inherit 'markdown-header-face :height 2.0)
            (set-face-attribute 'markdown-header-face-2 nil :inherit 'markdown-header-face :height 1.5)
            (set-face-attribute 'markdown-header-face-3 nil :inherit 'markdown-header-face :height 1.3)
            (set-face-attribute 'markdown-header-face-4 nil :inherit 'markdown-header-face :height 1.2)
            (set-face-attribute 'markdown-header-face-5 nil :inherit 'markdown-header-face :height 1.1)))

(req-package org
  :bind ("\C-c a" . org-agenda)
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
            (set-face-attribute 'org-headline-done nil :foreground "#859900")
            (set-face-attribute 'org-level-1 nil :foreground "#ffffff" :height 1.5)
            (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :foreground "#ffffff")
            (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :foreground "#ffffff")))

(req-package yasnippet
  :require (yasnippet-snippets)
  :config (yas-global-mode))

(req-package diff-hl
  :config (global-diff-hl-mode))

(req-package popwin
  :config (progn
            (popwin-mode 1)
            (global-set-key (kbd "C-z") popwin:keymap)))

(req-package projectile
  :require (helm-projectile helm-ag)
  :bind (("C-c h" . helm-projectile)
         ("C-c p" . projectile-command-map))
  :config (progn
            (projectile-global-mode)
            (helm-projectile-on)
            (setq projectile-use-git-grep 1)))


(req-package multiple-cursors

  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-M-c" . mc/edit-lines)))

(req-package python
  :require (blacken isortify)
  :config (progn
            (add-hook 'python-mode-hook 'blacken-mode)
            (add-hook `python-mode-hook 'isortify-mode)))

(req-package terraform-mode
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))
