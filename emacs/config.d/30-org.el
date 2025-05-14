(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org
  :after emacsql-sqlite-module
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("RET" . org-return-and-maybe-indent))

  :hook (org-mode . variable-pitch-mode)

  :config
  (setq org-modules '(ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww))
  (setq org-hide-emphasis-markers t)
  (setq org-adapt-indentation nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-hidden-keywords '(title))

  :custom-face
  (org-level-1 ((t (:inherit 'outline-1 :height 2.0))))
  (org-level-2 ((t (:inherit 'outline-2 :height 1.5))))
  (org-level-3 ((t (:inherit 'outline-3 :height 1.3))))
  (org-level-4 ((t (:inherit 'outline-4 :height 1.2))))
  (org-level-5 ((t (:inherit 'outline-5 :height 1.1))))
  (org-block ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit (shadow fixed-pitch)))))
  (org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  (org-document-title ((t (:height 2.5))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-property-value ((t (:inherit fixed-pitch))))
  (org-quote ((t (:inherit variable-pitch))))
  (org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-table ((t (:inherit fixed-pitch))))
  (org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.9))))
  (org-verbatim ((t (:inherit (shadow fixed-pitch))))))


(use-package org-roam
  :custom
  (org-roam-file-exclude-regexp '("data/" ".trash/" "Archive/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "Inbox/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${type:20} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))


(use-package org-roam-dailies
  :straight nil
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :config

  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n")))))

(use-package org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
