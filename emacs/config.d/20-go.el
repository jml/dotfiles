(req-package go-mode
  :config
  (progn (add-hook 'before-save-hook 'gofmt-before-save)
         (add-hook 'go-mode-hook (lambda () (local-set-key (kbd "M-.") #'godef-jump)))))
