# Emacs Configuration Organization Conventions

This document outlines the organizational conventions for this Emacs configuration.

## File Organization

### Numbering Scheme (10-unit groupings)

- **00-09: Core & Bootstrap**
  - Fundamental Emacs setup, core settings, keybindings
  - Examples: bootstrap, core-settings, keybindings, backups, encoding

- **10-19: UI & Appearance**
  - Visual appearance, themes, UI enhancements
  - Examples: appearance, ui-enhancements, themes

- **20-29: Navigation & Project Management**
  - File navigation, project management, search tools
  - Examples: navigation (helm/ivy), project-tools (projectile/treemacs)

- **30-39: Development Tools & LSP**
  - General development tools, LSP configuration, version control
  - Examples: development (flycheck, snippets), lsp, version-control, terminal

- **40-49: Language-Specific Configuration**
  - Programming language modes and their specific tools
  - Examples: python, haskell, go, rust, web-languages, infrastructure

- **50-59: Document & Note Management**
  - Document editing, note-taking, knowledge management
  - Examples: org, markdown, obsidian

- **60-69: Platform & System Integration**
  - OS-specific configuration, system integration
  - Examples: macos, environment (PATH, shell integration)

## File Structure Standards

### File Headers
Every configuration file should follow this header template:

```elisp
;;; NN-filename.el --- Brief Description

;;; Commentary:
;; Detailed explanation of what this file configures
;; Include any important notes about dependencies or setup requirements

;;; Code:

[configuration content]

;;; NN-filename.el ends here
```

### Use-Package Standards

#### Built-in Packages
```elisp
(use-package package-name
  :ensure nil  ; Built-in package
  :custom
  (setting-name value)
  :hook
  (mode-name . function)
  :config
  (additional-setup))
```

#### External Packages
```elisp
(use-package package-name
  :custom
  (setting-name value)
  :hook
  (mode-name . function)
  :bind
  ("key" . command)
  :config
  (additional-setup))
```

### Language Configuration Template
Each language-specific file should follow this structure:

```elisp
;;; NN-language.el --- Language Development Configuration

;;; Commentary:
;; Configuration for [Language] development including:
;; - Basic mode setup
;; - LSP integration
;; - Language-specific tools and utilities

;;; Code:

;; Basic mode configuration
(use-package language-mode
  :mode "\\.[extension]\\'"
  :custom
  (language-specific-settings value))

;; LSP integration
(use-package lsp-language
  :hook (language-mode . lsp)
  :custom
  (lsp-language-setting value))

;; Additional language tools
(use-package language-tool
  :after language-mode
  :config
  (setup-code))

;;; NN-language.el ends here
```

## Package Grouping Guidelines

### UI Enhancements (10-19)
- Visual improvements that don't affect core functionality
- Multiple cursors, line highlighting, visual indicators
- Should be easily disableable without breaking functionality

### Navigation (20-29)
- Tools for finding and moving between files/buffers
- Project management and workspace organization
- Search and completion frameworks

### Development Tools (30-39)
- Language-agnostic development utilities
- Linting, syntax checking, snippets
- Version control, terminal integration
- LSP base configuration (language-specific LSP goes with languages)

### Languages (40-49)
- One file per major language or related language group
- Include basic mode, LSP integration, and language-specific tools
- Group related languages (e.g., web-languages.el for HTML/CSS/JS)

## Dependency Management

### Load Order
- Files are loaded alphabetically by filename
- Lower numbers load first, establishing foundations for higher numbers
- Language files (40-49) can depend on development tools (30-39) being loaded

### Cross-File Dependencies
- Avoid circular dependencies between configuration files
- Use `:after` in use-package declarations when packages depend on others
- Document significant dependencies in file commentary

## Migration Guidelines

When reorganizing existing configuration:

1. **Preserve existing functionality** - ensure all packages remain configured
2. **Test incrementally** - move small groups of related packages at a time  
3. **Update documentation** - adjust commentary to reflect new organization
4. **Maintain git history** - prefer small, focused commits over large reorganizations

## Naming Conventions

### File Names
- Use descriptive names that clearly indicate content
- Prefer specific names over generic ones
- Examples: `42-go.el` not `42-languages.el`, `20-navigation.el` not `20-tools.el`

### Package Groupings
- Group packages by **functionality** not just by similar names
- Consider **usage patterns** - packages often used together should be in the same file
- Separate **concerns** - don't mix UI improvements with development tools

## Future Expansion

When adding new packages:

1. **Identify the appropriate category** (00-69 numbering scheme)
2. **Check if a suitable file exists** in that category
3. **Create new files sparingly** - prefer adding to existing logical groupings
4. **Update this document** if adding new organizational patterns

This organization prioritizes:
- **Logical grouping** over alphabetical sorting
- **Functional separation** over file size optimization  
- **Maintainability** over initial setup simplicity
- **Scalability** for future additions