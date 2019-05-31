(req-package projectile
  :require (helm-projectile helm-ag)
  :bind (("C-c h" . helm-projectile)
         ("C-c p" . projectile-command-map))
  :config (progn
            (projectile-global-mode)
            (helm-projectile-on)
            (setq projectile-use-git-grep 1)))
