(req-package popwin
  :config (progn
            (popwin-mode 1)
            (global-set-key (kbd "C-z") popwin:keymap)))
