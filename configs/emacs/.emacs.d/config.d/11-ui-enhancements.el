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

;; Built-in line numbers (Emacs 26+)
(global-display-line-numbers-mode)
(setq display-line-numbers-highlight-current-line t)

;; Show available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1))

;; Diminish minor modes from the mode line
(use-package diminish)

;;; 11-ui-enhancements.el ends here
