
;;; 62-custom-utilities.el --- Custom Utilities and Extensions

;;; Commentary:
;; Custom utilities and extensions specific to this configuration.
;; These are locally developed utilities that extend Emacs functionality
;; and are not available as standard packages.

;;; Code:

;; Make my custom utilities available
(use-package jml-utils
  :straight nil)

;; Make my org-roam extensions available
(use-package jml-org-roam-utils
  :bind-keymap ("C-c n j" . jml/org-roam-utils-map)
  :straight nil
  :config
  (add-to-list 'popwin:special-display-config
               '("*org-roam dailies summary*" :position left :width 80)))

;;; 62-custom-utilities.el ends here
