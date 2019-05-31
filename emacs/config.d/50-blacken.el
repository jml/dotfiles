(req-package python
  :require (blacken isortify)
  :config (progn
            (add-hook 'python-mode-hook 'blacken-mode)
            (add-hook `python-mode-hook 'isortify-mode)))
