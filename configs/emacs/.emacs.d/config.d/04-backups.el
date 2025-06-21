
;; Make sure we're not storing files all over the place.
(setq temporary-file-directory "~/.emacs.d/temp")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
