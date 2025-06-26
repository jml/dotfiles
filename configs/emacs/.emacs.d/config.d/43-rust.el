;;; 43-rust.el --- Rust Development Configuration

;;; Commentary:
;; Configuration for Rust development including:
;; - Basic Rust mode setup
;; - LSP integration with rust-analyzer
;; - Cargo integration for project management

;;; Code:

;; Basic Rust mode
(use-package rust-mode)

;; LSP integration for Rust
(use-package lsp-mode
  :hook (rust-mode . lsp)
  :commands lsp)

;; Cargo integration
(use-package cargo)

;;; 43-rust.el ends here