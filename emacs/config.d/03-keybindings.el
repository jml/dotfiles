;; Keybindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-m" 'newline-and-indent) ;; enter key indents
(global-set-key "\M-j" (lambda () (interactive) (delete-indentation 1)))
(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)
