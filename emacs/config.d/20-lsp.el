(req-package lsp-mode
  :hook (prog-mode . lsp)
  :commands lsp)

;; optionally
(req-package lsp-ui :commands lsp-ui-mode)
(req-package company-lsp :commands company-lsp)
(req-package helm-lsp :commands helm-lsp-workspace-symbol)
(req-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(req-package dap-mode)
