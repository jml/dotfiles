;; Swap Command and Option when running in graphical mode on macOS
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/178
(when (and (eq system-type 'darwin) (display-graphic-p))
  (cl-rotatef mac-command-modifier mac-option-modifier))
