;;; package --- Writing mode configuration
;;; Commentary:
;; Configuration for prose writing (novels, articles, etc.)

;;; Code:

;; Olivetti - Distraction-free writing with centered text
(use-package olivetti
  :hook (markdown-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 120))

;;; 30-writing.el ends here
