
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-use-header-line nil)
 '(auto-mode-case-fold nil)
 '(auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")
 '(background-mode dark)
 '(backup-by-copying-when-linked t)
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-default-macosx-browser)
 '(case-fold-search t)
 '(column-number-mode t)
 '(comment-auto-fill-only-comments t)
 '(compilation-message-face 'default)
 '(compilation-mode-hook '((lambda nil (toggle-truncate-lines nil))))
 '(compilation-scroll-output t)
 '(completion-cycle-threshold 5)
 '(current-language-environment "English")
 '(custom-buffer-indent 4)
 '(custom-safe-themes
   '("d13c66a22bb539c49515bbf74f911eeee39fcc6b6dc9b9bbe7168faa39c40607"
     "063c278e83aa631e230535f1be093fa57d0df4a2f5b7e781c6952e6145532976"
     "4b287bfbd581ea819e5d7abe139db1fb5ba71ab945cec438c48722bea3ed6689"
     "ed68393e901a88b9feefea1abfa9a9c5983e166e4378c71bb92e636423bd94fd"
     "e29a6c66d4c383dbda21f48effe83a1c2a1058a17ac506d60889aba36685ed94"
     "60940e1f2fa3f4e61e7a7ed9bab9c22676aa25f927d5915c8f0fa3a8bf529821"
     "dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d"
     "855eb24c0ea67e3b64d5d07730b96908bac6f4cd1e5a5986493cbac45e9d9636"
     "5f1bd7f67dc1598977e69c6a0aed3c926f49581fdf395a6246f9bc1df86cb030"
     "947190b4f17f78c39b0ab1ea95b1e6097cc9202d55c73a702395fc817f899393"
     "35b0b0e531731e270708ddb342dc2e576a31fb298dcbc56a206596a43afac54f"
     "bc75dfb513af404a26260b3420d1f3e4131df752c19ab2984a7c85def9a2917e"
     "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e"
     default))
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-fold-search nil)
 '(dabbrev-case-replace nil)
 '(dabbrev-upcase-means-case-search t)
 '(desktop-base-file-name ".emacs.d/.emacs.desktop")
 '(desktop-base-lock-name ".emacs.d/.emacs.desktop.lock")
 '(desktop-load-locked-desktop t)
 '(desktop-save t)
 '(ess-display-buffer-reuse-frames nil)
 '(ess-language "R" t)
 '(find-file-existing-other-name nil)
 '(flycheck-flake8rc ".flake8")
 '(flycheck-go-build-install-deps t)
 '(flycheck-go-golint-executable "/Users/jml/go/bin/golint")
 '(flycheck-hlint-language-extensions '("TypeApplications"))
 '(flymake-log-level -1)
 '(flymake-run-in-place nil)
 '(global-font-lock-mode t)
 '(global-linum-mode nil)
 '(godef-command "/Users/jml/go/bin/godef")
 '(gofmt-args '("-s" "-w"))
 '(grep-template "grep <C> -I -nH -e <R> <F>")
 '(gutter-buffers-tab-visible-p nil)
 '(haskell-program-name "ghci \"+.\"")
 '(hindent-reformat-buffer-on-save t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(kill-whole-line nil)
 '(line-number-mode t)
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(mail-envelope-from 'header)
 '(mail-specify-envelope-from t)
 '(markdown-command "markdown | smartypants")
 '(markdown-follow-wiki-link-on-enter nil)
 '(markdown-indent-on-enter t)
 '(menu-bar-mode nil)
 '(message-sendmail-envelope-from 'header)
 '(org-agenda-todo-ignore-scheduled 'future)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(paren-mode 'paren nil (paren))
 '(php-mode-speedbar-open nil)
 '(prog-mode-hook '((lambda nil (setq show-trailing-whitespace t))))
 '(query-user-mail-address nil t)
 '(require-final-newline t)
 '(rst-level-face-base-light 30)
 '(safe-local-variable-values
   '((flycheck-disabled-checkers quote (python-mypy))
     (haskell-process-use-ghci . t) (haskell-indent-spaces . 4)
     (whitespace-style face tabs spaces trailing lines space-before-tab::space
                       newline indentation::space empty space-after-tab::space
                       space-mark tab-mark newline-mark)
     (encoding . utf8) (encoding . utf-8)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 100000)
 '(scroll-down-aggressively nil)
 '(scroll-error-top-bottom t)
 '(scroll-step 0)
 '(scroll-up-aggressively nil)
 '(scrollbars-visible-p nil)
 '(select-enable-clipboard t)
 '(send-mail-function 'smtpmail-send-it)
 '(sendmail-program "/usr/bin/msmtp")
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-stream-type 'starttls)
 '(split-height-threshold nil)
 '(split-width-threshold nil)
 '(sql-pop-to-buffer-after-send-region t)
 '(tab-width 4)
 '(text-mode-hook
   '(text-mode-hook-identify visual-line-mode
                             (lambda nil (setq show-trailing-whitespace t))))
 '(tool-bar-mode nil nil (tool-bar))
 '(toolbar-visible-p nil)
 '(track-eol t)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify))
 '(user-mail-address "jml@mumak.net")
 '(vc-handled-backends '(RCS CVS SVN SCCS Git Hg Arch))
 '(vc-make-backup-files t)
 '(visible-bell t)
 '(which-function-mode t)
 '(wiki-follow-name-action 'find-file)
 '(wiki-name-regexp "\\<[A-Z][a-z]+\\([A-Z][a-z]+\\)+\\>"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:height 2.5))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))) t)
 '(org-level-1 ((t (:inherit 'outline-1 :height 2.0))))
 '(org-level-2 ((t (:inherit 'outline-2 :height 1.5))))
 '(org-level-3 ((t (:inherit 'outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit 'outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit 'outline-5 :height 1.1))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-quote ((t (:inherit variable-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.9))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(rst-level-1 ((t :height 2.0)))
 '(rst-level-2 ((t :height 1.5)))
 '(rst-level-3 ((t :height 1.3)))
 '(rst-level-4 ((t :height 1.2)))
 '(rst-level-5 ((t :height 1.1))))
