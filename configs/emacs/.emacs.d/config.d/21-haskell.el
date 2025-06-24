;;; 21-haskell.el --- Haskell configuration

;; Haskell

(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :custom
  (haskell-font-lock-symbols t)
  (haskell-indentation-show-indentations-after-eol nil)
  (haskell-program-name "ghci \"+.\""))

(use-package lsp-haskell
  :hook (haskell-mode . lsp)
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server")
  (setq lsp-haskell-process-args-hie '()))
  ;; Comment/uncomment this line to see interactions between lsp client/server.
  ;;(setq lsp-log-io t)

;;; 21-haskell.el ends here
