;;; 20-navigation.el --- Navigation and Project Management

;;; Commentary:
;; Tools for finding and moving between files, buffers, and projects.
;; Includes completion frameworks, project management, and search tools.

;;; Code:

;; Vertico - vertical completion UI
(use-package vertico
  :init
  (vertico-mode)
  :config
  ;; Cycle from bottom to top
  (setq vertico-cycle t))

;; Orderless - flexible completion matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Consult - enhanced commands with completion
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s r" . consult-ripgrep)
         ("M-s g" . consult-git-grep)
         ("M-s f" . consult-find)
         ("C-x C-f" . find-file))
  :config
  ;; Use project.el for Consult commands
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;; Include hidden files in ripgrep searches, but exclude .git
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip --hidden --glob !.git"))

;; Embark - context actions on completion candidates
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  ;; Hide modeline when embark is active
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark integration with Consult
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Project management
(use-package projectile
  :bind (("C-c h" . projectile-find-file)
         ("C-c p p" . projectile-switch-project))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
  (setq projectile-use-git-grep 1))

;; M-x now uses Vertico with native execute-extended-command
(global-set-key (kbd "M-x") 'execute-extended-command)

;; Rich annotations for completion
(use-package marginalia
  :config
  (marginalia-mode))

;; Popup window management
(use-package popwin
  :config
  (popwin-mode 1))

;;; 20-navigation.el ends here
