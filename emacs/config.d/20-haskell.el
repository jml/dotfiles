
(req-package haskell-mode
  :require (haskell-process ghc-nix haskell-style intero)
  :config (progn
            (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)
            ;; intero-mode stupidly slow on Haskell scripts
            (add-hook 'haskell-mode-hook #'intero-mode)
;;            (add-hook 'haskell-mode-hook #'use-nix-ghc-in-flycheck)
;;            (global-intero-mode)
            (flycheck-add-next-checker 'intero
                                       '(warning . haskell-hlint))))


;; See also (add-hook 'haskell-mode-hook #'haskell-style) for switching on johanTibell style

;; TODO: hindent set up
