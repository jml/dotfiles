;; Markdown — Tufte-inspired typography
;;
;; Mirrors the typographic choices from Tufte CSS: ET Book / Palatino body,
;; lightweight headings (h2/h3 italic, never bold), cream background, and a
;; narrow measure (~70 chars via olivetti in 30-writing.el).

;; Tufte palette — named constants for every color used in the theme.
(defconst jml/tufte-bg         "#fffff8" "Cream page background.")
(defconst jml/tufte-fg         "#111111" "Body text, near-black.")
(defconst jml/tufte-fg-mid     "#444444" "Mid-gray, used for types.")
(defconst jml/tufte-fg-dim     "#555555" "Dim text, used for builtins.")
(defconst jml/tufte-fg-faint   "#888888" "Faint text, comments and line numbers.")
(defconst jml/tufte-red        "#770000" "Dark red, used for keywords.")
(defconst jml/tufte-green      "#2a6a2a" "Forest green, used for strings.")
(defconst jml/tufte-purple     "#553388" "Muted purple, used for constants.")

(defun jml/markdown-tufte-faces ()
  "Buffer-local face remaps that approximate Tufte CSS."
  (face-remap-add-relative 'default
                           :background jml/tufte-bg
                           :foreground jml/tufte-fg)
  (face-remap-add-relative 'fringe
                           :background jml/tufte-bg)
  (face-remap-add-relative 'variable-pitch
                           :family "Georgia"
                           :height 160)
  ;; Code faces: monospace, slightly smaller than body (Tufte uses 0.9x for pre>code).
  ;; Must set :family explicitly since default now inherits variable-pitch.
  ;; Foreground matches body text — Tufte CSS applies no special color to code.
  (face-remap-add-relative 'fixed-pitch
                           :family "Source Code Pro"
                           :foreground jml/tufte-fg
                           :height 0.95)
  (face-remap-add-relative 'markdown-code-face      :foreground jml/tufte-fg)
  (face-remap-add-relative 'markdown-pre-face       :foreground jml/tufte-fg)
  (face-remap-add-relative 'markdown-wiki-link-face :foreground jml/tufte-red :underline nil)
  ;; Tufte headings: all weight-normal, never bold. h1 is upright;
  ;; h2–h5 use italics for hierarchy instead of weight.
  ;; face-remap-set-base completely replaces the face definition buffer-locally,
  ;; which reliably overrides markdown-mode's default bold weight.
  (face-remap-set-base 'markdown-header-face
                       :weight 'normal :foreground jml/tufte-fg :inherit 'variable-pitch)
  (face-remap-set-base 'markdown-header-face-1
                       :inherit 'markdown-header-face :height 2.8 :slant 'normal)
  (face-remap-set-base 'markdown-header-face-2
                       :inherit 'markdown-header-face :height 2.0 :slant 'italic)
  (face-remap-set-base 'markdown-header-face-3
                       :inherit 'markdown-header-face :height 1.5 :slant 'italic)
  (face-remap-set-base 'markdown-header-face-4
                       :inherit 'markdown-header-face :height 1.2 :slant 'italic)
  (face-remap-set-base 'markdown-header-face-5
                       :inherit 'markdown-header-face :height 1.0 :slant 'italic)
  ;; Disable hl-line and line numbers — both clash with the cream background.
  (setq-local global-hl-line-mode nil)
  (hl-line-mode -1)
  (display-line-numbers-mode -1)
  ;; Override font-lock faces so syntax highlighting works on a light background.
  ;; Muted palette inspired by Tufte's restraint — no bright or saturated colors.
  (face-remap-add-relative 'font-lock-keyword-face       :foreground jml/tufte-red)
  (face-remap-add-relative 'font-lock-string-face        :foreground jml/tufte-green)
  (face-remap-add-relative 'font-lock-comment-face       :foreground jml/tufte-fg-faint :slant 'italic)
  (face-remap-add-relative 'font-lock-function-name-face :foreground jml/tufte-fg)
  (face-remap-add-relative 'font-lock-variable-name-face :foreground jml/tufte-fg-dim)
  (face-remap-add-relative 'font-lock-type-face          :foreground jml/tufte-fg-mid)
  (face-remap-add-relative 'font-lock-constant-face      :foreground jml/tufte-purple)
  (face-remap-add-relative 'font-lock-builtin-face       :foreground jml/tufte-fg-dim)
  ;; Line spacing: Tufte CSS uses line-height 1.43 on a 21px body.
  ;; At ~16pt in Emacs, 6px extra leading is roughly equivalent.
  (setq-local line-spacing 6)
  ;; Hide the modeline for distraction-free prose
  (setq-local mode-line-format nil))

(use-package markdown-mode
  :mode (("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))

  :hook ((markdown-mode . variable-pitch-mode)
         (markdown-mode . jml/markdown-tufte-faces))

  :custom
  (markdown-command "markdown | smartypants")
  (markdown-enable-wiki-links t)
  (markdown-follow-wiki-link-on-enter t)
  (markdown-wiki-link-search-type '(sub-directories parent-directories))
  (markdown-indent-on-enter t)
  (markdown-hide-markup t)

  :config
  (custom-set-faces
   '(markdown-table-face ((t (:inherit fixed-pitch))))
   '(markdown-code-face  ((t (:inherit fixed-pitch :background unspecified))))
   '(markdown-pre-face   ((t (:inherit fixed-pitch :background unspecified))))))
