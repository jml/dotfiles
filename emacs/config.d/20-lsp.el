(use-package lsp-mode
  :hook (rust-mode . lsp)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-diagnostic-mode "workspace"))

(use-package lsp-ui
  :after (diminish flycheck lsp-mode)
  :diminish eldoc-mode
  :config
  (setq lsp-ui-doc-use-webkit t)
  :commands (lsp-ui-mode lsp)
  :custom
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-position (quote at-point))
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-hover t))

;; Company for completion
(use-package company
  :ensure t
  :hook (python-mode . company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1))

;; TODO(jml): What is this for?
(use-package company-box
  :after diminish
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))


;; Debug Adapter Protocol for debugging
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

