;; Use Adobe's Source Code Pro.
(set-face-attribute 'default nil :height 120 :foundry "adobe" :family "Source Code Pro")
(set-face-attribute 'fixed-pitch nil :foundry "adobe" :family "Source Code Pro")
(set-face-attribute 'variable-pitch nil :height 140 :family "Source Sans Pro")

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; UI appearance settings
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq visible-bell t)
(setq initial-scratch-message nil)
(setq blink-cursor-mode nil)
(show-paren-mode t)
(setq show-paren-style 'expression)
(column-number-mode t)
(line-number-mode t)
(which-function-mode t)

;; Give us a bit of breathing room between lines.
;; Experimenting shows that VS Code uses line spacing of 3.
(setq-default line-spacing 2)

;; Theme

;; I don't ever inspect the code on upgrade. Themes are the same as any other Emacs package for me.
(setq custom-safe-themes t)

(use-package solaire-mode
 :hook
 ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
 :config
 (solaire-global-mode +1))

(use-package all-the-icons)

(use-package doom-themes
  :after all-the-icons
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-spacegrey t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  (setq doom-themes-treemacs-theme "doom-atom") ; use the colorful treemacs theme
  (doom-themes-treemacs-config))

;; Doom Modeline
;;
;; Modern, performant mode line with rich LSP and Git indicators
(use-package diminish)

(use-package doom-modeline
  :after all-the-icons
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github t)
  (setq doom-modeline-env-version t))
