;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; This is only needed once, near the top of the file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Customize
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Load manually-managed third-party plugins.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))

(require 'load-directory)

;; Configuration that's appropriate in all the places I use Emacs.
(load-directory (expand-file-name "~/.emacs.d/config.d"))

;; Configuration that's only appropriate in this installation.
(load-directory (expand-file-name "~/.emacs.d/local.d"))
