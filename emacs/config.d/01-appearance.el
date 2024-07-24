;; Use Adobe's Source Code Pro.
(set-face-attribute 'default nil :height 120 :foundry "adobe" :family "Source Code Pro")
(set-face-attribute 'fixed-pitch nil :foundry "adobe" :family "Source Code Pro")
(set-face-attribute 'variable-pitch nil :height 140 :family "Source Sans Pro")

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; Give us a bit of breathing room between lines.
;; Experimenting shows that VS Code uses line spacing of 3.
(setq-default line-spacing 2)
