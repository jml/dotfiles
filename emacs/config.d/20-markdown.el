
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
