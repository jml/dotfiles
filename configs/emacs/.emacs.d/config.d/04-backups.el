
;; Make sure we're not storing files all over the place.
(setq temporary-file-directory "~/.emacs.d/temp")
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Additional backup settings
(setq backup-by-copying-when-linked t)
(setq vc-make-backup-files t)
