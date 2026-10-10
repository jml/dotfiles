;;; 61-environment.el --- System Environment Integration

;;; Commentary:
;; Integration with system environment including:
;; - PATH and environment variable management
;; - Shell integration and environment synchronization
;; - Directory-local environment management

;;; Code:

;; Import environment variables from shell. Must not gate on `window-system'
;; because when launched via `emacsclient -a ""' the daemon starts with no
;; frame, leaving `window-system' nil during init.
(use-package exec-path-from-shell
  :demand t
  :config (exec-path-from-shell-initialize))

;; Directory-based environment management
(use-package direnv)

;;; 61-environment.el ends here