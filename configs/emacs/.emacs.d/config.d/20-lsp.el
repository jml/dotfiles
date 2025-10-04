(use-package lsp-mode
  :hook (rust-mode . lsp)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-diagnostic-mode "workspace"))

(use-package lsp-ui
  :after (diminish flycheck lsp-mode)
  :diminish eldoc-mode
  :commands (lsp-ui-mode lsp)
  :custom
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-position (quote at-point))
  (lsp-ui-doc-use-childframe nil)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-hover t))

;; Corfu for completion
;; Lightweight completion popup using child frames
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                    ; Enable auto completion
  (corfu-auto-delay 0.1)            ; Delay before showing completions
  (corfu-auto-prefix 1)             ; Minimum prefix length
  (corfu-cycle t)                   ; Enable cycling through candidates
  (corfu-preview-current nil)       ; Don't preview current candidate
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

;; Corfu popup styling with icons
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Debug Adapter Protocol for debugging
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

