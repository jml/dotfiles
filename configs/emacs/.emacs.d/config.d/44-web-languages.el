;;; 44-web-languages.el --- Web and Markup Language Configuration

;;; Commentary:
;; Configuration for web development and markup languages including:
;; - Web development modes (HTML, CSS, JavaScript)
;; - Configuration and data formats (YAML, TOML, JSON)
;; - Markup and documentation formats
;; - Protocol buffer definitions

;;; Code:

;; Web development
(use-package web-mode)

;; YAML configuration files
(use-package yaml-mode)

;; TOML configuration files
(use-package toml-mode)

;; Protocol Buffer definitions
(use-package protobuf-mode)

;; Groovy language support
(use-package groovy-mode)

;; GraphViz DOT language
(use-package graphviz-dot-mode)

;; reStructuredText markup
(use-package rst
  :hook (rst-mode . variable-pitch-mode)

  :custom-face
  (rst-level-1 ((t :height 2.0)))
  (rst-level-2 ((t :height 1.5)))
  (rst-level-3 ((t :height 1.3)))
  (rst-level-4 ((t :height 1.2)))
  (rst-level-5 ((t :height 1.1))))

;;; 44-web-languages.el ends here