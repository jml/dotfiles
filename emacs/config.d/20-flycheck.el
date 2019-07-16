;; Enable flycheck globally

(req-package flycheck
  :config
  (progn
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

(req-package flycheck-gometalinter
  :require (flycheck)
  :config
  (progn
    (flycheck-gometalinter-setup)
    (setq flycheck-gometalinter-vendor t)
    (setq flycheck-gometalinter-fast t)))
