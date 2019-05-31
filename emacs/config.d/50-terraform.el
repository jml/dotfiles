(req-package terraform-mode
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))
