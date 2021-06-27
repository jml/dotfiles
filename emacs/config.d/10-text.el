;; If we don't know what mode to use, use text-mode.
;; fundamental-mode isn't very useful.
(setq-default major-mode 'text-mode)

;; TODO(jml): Have yet to test either of these successfully.
;; Initial experimentation shows that visual-line-mode not triggered by default,
;; but that could be due to bad experimenting.
;;
;;(setq sentence-end-double-space nil)
;;(add-hook 'text-mode-hook #'visual-line-mode)
