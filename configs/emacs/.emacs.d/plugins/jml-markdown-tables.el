;;; jml-markdown-tables.el --- Prettier markdown tables -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Prettier markdown tables: an italic header row, and a sticky header line
;;; that keeps column names visible while scrolling through a long table.
;;;
;;; Code:

(declare-function markdown-table-at-point-p "markdown-mode")
(declare-function markdown-table-begin "markdown-mode")

(defface jml/markdown-table-header-face
  '((t :inherit markdown-table-face :slant italic))
  "Face for the header row of a markdown table.
Italic rather than bold, matching the Tufte convention of marking
hierarchy with slant instead of weight.")

;; A header row is a pipe row immediately followed by the separator row
;; (dashes with optional alignment colons).  The match spans two lines, so
;; buffers using this need `font-lock-multiline' enabled.
(defconst jml/markdown-table--header-regexp
  "^[ \t]*\\(|.*|\\)[ \t]*\n[ \t]*|\\(?:[ \t]*:?-+:?[ \t]*|\\)+[ \t]*$"
  "Regexp matching a table header row followed by its separator row.")

;;;###autoload
(defun jml/markdown-tables-fontify ()
  "Give the header row of markdown tables a distinct face."
  (setq-local font-lock-multiline t)
  (font-lock-add-keywords
   nil
   `((,jml/markdown-table--header-regexp 1 'jml/markdown-table-header-face prepend))
   'append))

(defun jml/markdown-table--offscreen-header ()
  "Return the header row of the table scrolled past the top of the window.
Return nil unless the window starts inside a table whose header row is
off-screen.  The returned text is raw (valign's pixel alignment does not
transfer to the header line), but it keeps the column names in view."
  (save-excursion
    (goto-char (window-start))
    (when (and (markdown-table-at-point-p)
               (< (markdown-table-begin) (point)))
      (goto-char (markdown-table-begin))
      (propertize
       (concat "  " (buffer-substring-no-properties (point) (line-end-position)))
       'face 'jml/markdown-table-header-face))))

;;;###autoload
(defun jml/markdown-table-header-line ()
  "Header line construct: pin the table header while scrolling a long table.
Falls back to the buffer name, the usual running head."
  (or (jml/markdown-table--offscreen-header) "  %b"))

(provide 'jml-markdown-tables)
;;; jml-markdown-tables.el ends here
