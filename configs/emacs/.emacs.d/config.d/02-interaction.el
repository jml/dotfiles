;;; package --- Summary
;;; Commentary:

;;; Code:

;; Make all "yes or no" prompts "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Scrolling behavior
(setq scroll-step 1)
(setq scroll-conservatively 100000)
(setq scroll-error-top-bottom t)
(setq scroll-down-aggressively nil)
(setq scroll-up-aggressively nil)

;; Fill width
(setq-default fill-column 78)

(put 'narrow-to-region 'disabled nil)

;; Load dired-x.
(add-hook 'dired-load-hook (lambda () (load "dired-x")))

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

;; Allow any value for the test-case-name buffer local variable
(put 'test-case-name 'safe-local-variable 'identity)

(put 'scroll-left 'disabled nil)

;; Disable the damn bell
(setq ring-bell-function 'ignore)

;; If we don't know what mode to use, use text-mode.
;; fundamental-mode isn't very useful.
(setq-default major-mode 'text-mode)

;; General editing settings
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(setq kill-whole-line nil)
(setq track-eol t)
(setq truncate-lines t)
(transient-mark-mode t)
(setq case-fold-search t)
(setq select-enable-clipboard t)
(setq completion-cycle-threshold 5)
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; Buffer naming
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Dabbrev settings
(setq dabbrev-case-distinction nil)
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)
(setq dabbrev-upcase-means-case-search t)


;; Compilation settings
(setq compilation-message-face 'default)
(setq compilation-scroll-output t)
(setq comment-auto-fill-only-comments t)

;; Version control
(setq vc-handled-backends '(RCS CVS SVN SCCS Git Hg Arch))

;; Grep and search
(setq grep-template "grep <C> -I -nH -e <R> <F>")

;; Other settings
(setq auto-mode-case-fold nil)
(setq find-file-existing-other-name nil)
(setq Buffer-menu-use-header-line nil)
(setq custom-buffer-indent 4)
(setq sql-pop-to-buffer-after-send-region t)

;; Safe local variables
(setq safe-local-variable-values
      '((flycheck-disabled-checkers quote (python-mypy))
        (haskell-process-use-ghci . t)
        (haskell-indent-spaces . 4)
        (whitespace-style face tabs spaces trailing lines space-before-tab::space
                          newline indentation::space empty space-after-tab::space
                          space-mark tab-mark newline-mark)
        (encoding . utf8)
        (encoding . utf-8)))

;; Built-in mode configurations
(use-package prog-mode
  :straight nil
  :hook (prog-mode . (lambda () (setq show-trailing-whitespace t))))

(use-package text-mode
  :straight nil
  :hook ((text-mode . text-mode-hook-identify)
         (text-mode . visual-line-mode)
         (text-mode . (lambda () (setq show-trailing-whitespace t)))))

(use-package compile
  :ensure nil
  :hook (compilation-mode . (lambda () (toggle-truncate-lines nil))))

;;; 02-interaction.el ends here
