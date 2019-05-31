(req-package helm-config
  :config (progn
            (helm-mode 1)
            (global-set-key (kbd "M-x") 'helm-M-x)
            (global-set-key (kbd "C-x C-f") 'helm-find-files)))
