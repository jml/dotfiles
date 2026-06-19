;;; package --- Writing mode configuration
;;; Commentary:
;; Configuration for prose writing (novels, articles, etc.)

;;; Code:

;; Olivetti - Distraction-free writing with centered text
(use-package olivetti
  :hook (markdown-mode . olivetti-mode)
  :config
  ;; `olivetti-body-width' is automatically buffer-local, so a plain `setq'
  ;; would only affect the current buffer. Set the default so every buffer
  ;; that enables olivetti picks up 120 columns without manual adjustment.
  (setq-default olivetti-body-width 120))

;;; 30-writing.el ends here
