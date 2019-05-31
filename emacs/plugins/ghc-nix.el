;;; ghc-nix --- Tools for working better with ghc under nix
;;;
;;; Commentary:
;;;   Not much to see.
;;; Code:


(defun refresh-nix-package-databases ()
  "Update .dir-locals.el to have correct package databases for nix."
  (interactive)
  (add-dir-local-variable 'haskell-mode 'flycheck-ghc-package-databases (nix-package-databases))
  (add-dir-local-variable 'haskell-mode 'flycheck-haskell-ghc-executable (nix-ghc-executable)))

(defun use-nix-ghc-in-flycheck ()
  "Configure the current buffer to use ghc provided by the containing nix shell."
  (interactive)
  ;; Only set these variables if we detect that we're under a shell.nix
  (if (locate-dominating-file default-directory "shell.nix")
      (progn
        ;; If we have shell.nix, we don't want to even bother with stack, at
        ;; least, not until stack sorts out its ghc 8.0 support.
        (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
        ;; Setting package databases to nil seems to DTRT on ghc 8.0
        (setq flycheck-ghc-package-databases nil)
        (setq flycheck-haskell-ghc-executable (nix-ghc-executable))) nil))

(defun nix-package-databases ()
  "Return a list of ghc package databases under nix."
  (-find-package-databases (-run-nix-command "$(type -p ghc-pkg) list")))

(defun nix-ghc-executable ()
  "Return the path to the ghc executable inside the nix-shell."
  (-chomp-end (-run-nix-command "type -p ghc")))


(defun -run-nix-command (command)
  "Run COMMAND inside a nix-shell, returning output."
  (let ((default-directory (locate-dominating-file default-directory "shell.nix")))
    (shell-command-to-string (-get-command-for-env default-directory command))))

(defun -get-command-for-env (directory command)
  "Get the shell command to run COMMAND with correct environment."
  (let ((direnv-directory (locate-dominating-file directory ".envrc")))
    (if direnv-directory
        (-get-direnv-nix-command direnv-directory command) (-get-nix-command command))))

(defun -get-nix-command (command)
  "Get the command to run COMMAND inside a nix-shell."
  (combine-and-quote-strings `("nix-shell" "--run" ,command)))

(defun -get-direnv-nix-command (directory command)
  "Get the command to run COMMAND inside a nix-shell using direnv variables."
  (combine-and-quote-strings `("direnv" "exec" ,directory "$(type -p nix-shell)" "--command" ,command "2>/dev/null")))


(defun -find-package-databases (ghc-pkg-output)
  "Find package database paths in GHC-PKG-OUTPUT."
  (-map-maybe '-package-database-path (split-string ghc-pkg-output)))

(defun -package-database-p (s)
  "Is S a package database?"
  (char-equal (-first-element s) ?/))

(defun -package-database-path (s)
  "Return path to package database represented by S.

If not a package database, return nil."
  (if (-package-database-p s) s nil))

(defun -first-element (s)
  "Find the last element of S."
  (elt s 0))

(defun -map-maybe (f xs)
  "Map F over XS, discarding when f(x) is nil."
  (delq nil (mapcar f xs)))


(defun -chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))


(provide 'ghc-nix)

;;; (require 'ghc-nix)
;;; ghc-nix.el ends here
