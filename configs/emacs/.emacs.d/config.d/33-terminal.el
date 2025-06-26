;;; 33-terminal.el --- Terminal Integration

;;; Commentary:
;; Terminal emulation and integration within Emacs.
;; Provides access to shell and terminal functionality without leaving Emacs.

;;; Code:

;; vterm - high-performance terminal emulator
;;
;; https://github.com/akermu/emacs-libvterm
;;
;; Make sure you do https://github.com/akermu/emacs-libvterm#shell-side-configuration also
(use-package vterm
  :config
  (setq vterm-always-compile-module t))

;;; 33-terminal.el ends here