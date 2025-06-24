;;; 11-ui-enhancements.el --- UI Enhancement Packages

;;; Commentary:
;; Visual improvements and UI enhancements that don't affect core functionality.
;; These packages improve the visual experience and provide better feedback
;; but can be disabled without breaking essential Emacs functionality.

;;; Code:

;; Multiple cursors for editing multiple locations simultaneously
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-M-c" . mc/edit-lines)))

;; Highlight changes in version control
(use-package diff-hl
  :config (global-diff-hl-mode))

;; More efficient line numbers
(use-package nlinum
  :config
  (global-nlinum-mode)
  (setq nlinum-highlight-current-line t))

;; Diminish minor modes from the mode line
(use-package diminish)

;;; 11-ui-enhancements.el ends here