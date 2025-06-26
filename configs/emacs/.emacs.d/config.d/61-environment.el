;;; 61-environment.el --- System Environment Integration

;;; Commentary:
;; Integration with system environment including:
;; - PATH and environment variable management
;; - Shell integration and environment synchronization
;; - Directory-local environment management

;;; Code:

;; Import environment variables from shell on macOS
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;; Directory-based environment management
(use-package direnv)

;;; 61-environment.el ends here