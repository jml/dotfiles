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

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package all-the-icons)

(use-package doom-themes
  :after all-the-icons
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)


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

;; More efficient way of showing line numbers
(use-package nlinum
  :config
  (global-nlinum-mode)
  (setq nlinum-highlight-current-line t))

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
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos)

(use-package forge
  :after magit)

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
  :demand t
  :config
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

;; LSP

(use-package lsp-mode
  :hook
  ((rust-mode . lsp))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :after (diminish flycheck lsp-mode)
  :diminish eldoc-mode
  :config
  (setq lsp-ui-doc-use-webkit t)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-position (quote at-point))
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-hover t))

;; TODO: try this once python is working
;;(use-package lsp-treemacs
;;  :commands lsp-treemacs-errors-list)

(use-package company-lsp
  :after company
  :commands company-lsp)

(use-package company-box
  :after diminish
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

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
;; - memrise/webapp doesn't have stupid column length warnings
;; - memrise/webapp code uses memrise/webapp/mypy.ini for mypy config
;; - flycheck is enabled by default
;;
;; All we need to do is enable flycheck mode globally.

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
  :mode "\\.l?hs\\'"
  :custom
  (haskell-font-lock-symbols t)
  (haskell-indentation-show-indentations-after-eol nil))

(use-package lsp-haskell
  :hook (haskell-mode . lsp)
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server")
  (setq lsp-haskell-process-args-hie '()))
  ;; Comment/uncomment this line to see interactions between lsp client/server.
  ;;(setq lsp-log-io t)


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

;; (use-package blacken
;;   :delight
;;   :hook (python-mode . blacken-mode)
;;   :custom (blacken-only-if-project-is-blackened t))

;; (use-package pyvenv)

;; (use-package py-isort)

;; (use-package python-docstring)

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
