;; TODO: Move this to packages after verifying that environment variables
;; aren't needed earlier.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))
