;;; 45-infrastructure.el --- Infrastructure and DevOps Configuration

;;; Commentary:
;; Configuration for infrastructure, DevOps, and system configuration languages:
;; - Terraform for infrastructure as code
;; - Docker and containerization
;; - Nix package manager and system configuration

;;; Code:

;; Terraform infrastructure as code
(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

;; Docker containerization
(use-package dockerfile-mode)

;; Nix package manager
(use-package nix-mode)

;; Pretty SHA path display for Nix
(use-package pretty-sha-path)

;;; 45-infrastructure.el ends here