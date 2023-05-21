;; Swap Command and Option when running in graphical mode on macOS
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/178

;; This used to also check for (display-graphic-p), but because I always
;; launch Emacs as a daemon, this was never true, and so I ended up with
;; graphical frames with Command as <super> and Option as <meta>.
;;
;; What I *want* is for TTY frames to have Option as <meta> and graphical
;; frames to have Command as <meta>, but that's out of my grasp right now.

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'option)
  (global-unset-key (kbd "M-`")))
