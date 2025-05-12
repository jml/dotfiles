;; Markdown
(use-package markdown-mode
  :mode (("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))

  :hook (markdown-mode . variable-pitch-mode)

  :custom-face
  (markdown-header-face-1 ((t (:inherit 'markdown-header-face :height 2.0))))
  (markdown-header-face-2 ((t (:inherit 'markdown-header-face :height 1.5))))
  (markdown-header-face-3 ((t (:inherit 'markdown-header-face :height 1.3))))
  (markdown-header-face-4 ((t (:inherit 'markdown-header-face :height 1.2))))
  (markdown-header-face-5 ((t (:inherit 'markdown-header-face :height 1.1))))
  (markdown-table-face ((t (:inherit (shadow fixed-pitch)))))
  (markdown-code-face ((t (:inherit (shadow fixed-pitch)))))
  (markdown-pre-face ((t (:inherit (shadow fixed-pitch))))))


(use-package obsidian
  :config
  (global-obsidian-mode t)
  (obsidian-backlinks-mode t)
  :custom
  ;; Location of obsidian vault
  (obsidian-directory "/Users/jml/Library/Mobile Documents/iCloud~md~obsidian/Documents/Exobrain")
  ;; Default location for new notes from `obsidian-capture'
  (obsidian-inbox-directory "Inbox")
  ;; Useful if you're going to be using wiki links
  (markdown-enable-wiki-links t))


;; (define-key obsidian-mode-map (kbd "C-c M-o") 'obsidian-hydra/body)
