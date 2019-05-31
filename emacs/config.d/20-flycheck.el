;; Enable flycheck globally

(req-package flycheck
  :config
  (progn
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
    (add-hook 'after-init-hook #'global-flycheck-mode)))

(req-package flycheck-gometalinter
  :require (flycheck)
  :config
  (progn
    (flycheck-gometalinter-setup)
    (setq flycheck-gometalinter-vendor t)
    (setq flycheck-gometalinter-fast t)))
