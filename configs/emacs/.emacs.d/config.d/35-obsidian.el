;; Obsidian
(use-package obsidian
  :after (markdown elgrep)
  :custom
  ;; Default location for new notes from `obsidian-capture'
  (obsidian-inbox-directory "Inbox")
  ;; Useful if you're going to be using wiki links
  (markdown-enable-wiki-links t))

(use-package elgrep)

;; Obsidian TODO
;; - understand global-obsidian-mode and obsidian-backlinks-mode configuration
;; - set up Hydra
;;   (define-key obsidian-mode-map (kbd "C-c M-o") 'obsidian-hydra/body)
