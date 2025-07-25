;; Markdown
(use-package markdown-mode
  :mode (("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))

  :hook (markdown-mode . variable-pitch-mode)

  :custom
  (markdown-command "markdown | smartypants")
  (markdown-follow-wiki-link-on-enter nil)
  (markdown-indent-on-enter t)

  :config
  ;; We have to do this in :config rather than :customize-face,
  ;; otherwise it doesn't take effect. I don't understand the mechanism.
  (custom-set-faces
   '(markdown-header-face-1 ((t (:inherit 'markdown-header-face :height 2.0))))
   '(markdown-header-face-2 ((t (:inherit 'markdown-header-face :height 1.5))))
   '(markdown-header-face-3 ((t (:inherit 'markdown-header-face :height 1.3))))
   '(markdown-header-face-4 ((t (:inherit 'markdown-header-face :height 1.2))))
   '(markdown-header-face-5 ((t (:inherit 'markdown-header-face :height 1.1))))
   '(markdown-table-face ((t (:inherit (shadow fixed-pitch)))))
   '(markdown-code-face ((t (:inherit (shadow fixed-pitch)))))
   '(markdown-pre-face ((t (:inherit (shadow fixed-pitch)))))))
