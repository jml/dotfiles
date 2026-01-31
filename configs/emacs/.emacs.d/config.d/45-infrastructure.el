;;; 45-infrastructure.el --- Infrastructure and DevOps Configuration

;;; Commentary:
;; Configuration for infrastructure, DevOps, and system configuration languages:
;; - Terraform for infrastructure as code
;; - Container tooling (podman, Dockerfiles)
;; - Nix package manager and system configuration

;;; Code:

;; Terraform infrastructure as code
(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

;; Container images (Dockerfile syntax works for podman too)
(use-package dockerfile-mode)

;; Nix package manager
(use-package nix-mode)

;; Pretty SHA path display for Nix
(use-package pretty-sha-path)

;;; 45-infrastructure.el ends here