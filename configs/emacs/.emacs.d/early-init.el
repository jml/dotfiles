;; Remove the title bar and borders
;; Much more useful with tiling window managers
;; Drop `-round` for straight edges.
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; GUI launches (Aerospace, Dock, Spotlight) don't inherit shell PATH,
;; so libgccjit can't find gcc-15's runtime libs (libemutls_w, etc.) and
;; native compilation fails. Must run before any package loads, since
;; native-comp subprocesses inherit the environment at spawn time.
(let ((brew-bin "/opt/homebrew/bin"))
  (setenv "PATH" (concat brew-bin ":" (getenv "PATH")))
  (add-to-list 'exec-path brew-bin))
