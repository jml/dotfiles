;; Use Adobe's Source Code Pro.
(set-face-attribute 'default nil :height 120 :foundry "adobe" :family "Source Code Pro")
(set-face-attribute 'fixed-pitch nil :foundry "adobe" :family "Source Code Pro")
(set-face-attribute 'variable-pitch nil :height 140 :family "Source Sans Pro")

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; Give us a bit of breathing room between lines.
;; Experimenting shows that VS Code uses line spacing of 3.
(setq-default line-spacing 2)

;; Theme

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
