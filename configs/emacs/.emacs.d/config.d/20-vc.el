;; TODO(jml): Better understand dependency management for use-package
(use-package closql)
(use-package ghub)

(use-package magit
  :after (diminish transient magit-section closql ghub)
  :diminish auto-revert-mode
  :bind (("\C-c g g" . magit-status)
         ("<f12>" . magit-status))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  ;; Surface the branch description (edited via `b d`) in the status buffer,
  ;; not only the refs buffer, so a drafted PR body is visible while reviewing.
  (add-hook 'magit-status-headers-hook #'magit-insert-branch-description))

;; Target of the `magit' shell function in .zshrc, reached through
;; emacsclient --eval.  It calls `magit-status-setup-buffer' rather than
;; `magit-status': handed a directory below the repository root, the
;; interactive command would prompt to create a nested repository, and under
;; emacsclient that prompt would leave the shell hanging.
(declare-function magit-status-setup-buffer "magit-status")
(defun jml/visible-gui-frame-p (frame)
  "Return non-nil if FRAME is a graphical frame that is shown or iconified."
  (and (display-graphic-p frame) (frame-visible-p frame)))
(defun jml/magit-status-in-gui-frame (directory)
  "Show the Magit status buffer for DIRECTORY and bring Emacs to the front.
Prefer the selected frame, then any other visible graphical frame, and
create a frame only when there is none.  Tty frames and the daemon's initial
frame are skipped: a status buffer shown there would be invisible to
someone looking up from the shell."
  (let ((frame (or (seq-find #'jml/visible-gui-frame-p
                             (cons (selected-frame) (frame-list)))
                   (make-frame (and (featurep 'ns) '((window-system . ns)))))))
    (select-frame-set-input-focus frame)
    (magit-status-setup-buffer (file-name-as-directory directory))))

(use-package forge
  :after magit)

;; git-link
;;
;; Interactive Emacs functions that create URLs for files and commits in
;; GitHub repositories.
(use-package git-link)
