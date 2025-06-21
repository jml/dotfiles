;; Keybindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-m" 'newline-and-indent) ;; enter key indents
(global-set-key "\M-j" (lambda () (interactive) (delete-indentation 1)))
