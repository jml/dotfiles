;;; 20-navigation.el --- Navigation and Project Management

;;; Commentary:
;; Tools for finding and moving between files, buffers, and projects.
;; Includes completion frameworks, project management, and search tools.

;;; Code:

;; Helm - incremental completion and selection framework
(use-package helm
  :after diminish
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list))
  :config (helm-mode 1))

;; Ripgrep integration for Helm
(use-package helm-rg)

;; Helm integration for Projectile
(use-package helm-projectile)

;; Project management
(use-package projectile
  :after helm-projectile
  :bind ("C-c h" . helm-projectile)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; If we don't have this, we get tonnes of crazy arguments on the command-line.
  (setq helm-projectile-ignore-strategy 'search-tool)
  (projectile-global-mode)
  (helm-projectile-on)
  (setq projectile-use-git-grep 1))

;; Rich annotations for completion
(use-package marginalia
  :config
  (marginalia-mode))

;; Popup window management
(use-package popwin
  :config
  (popwin-mode 1))

;;; 20-navigation.el ends here