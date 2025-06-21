;; TODO(jml): Better understand dependency management for use-package
(use-package closql)
(use-package ghub)

(use-package magit
  :after (diminish transient magit-section closql ghub)
  :diminish auto-revert-mode
  :bind (("\C-c g g" . magit-status)
         ("<f12>" . magit-status))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

;; git-link
;;
;; Interactive Emacs functions that create URLs for files and commits in
;; GitHub repositories.
(use-package git-link)
