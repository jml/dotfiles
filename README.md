# dotfiles

jml's configuration

## Howto

### macOS

- [ ] Connect to the Internet
- [ ] Get Apple ID credentials from 1Password on phone to sign in
- [ ] Connect external keyboard (use physical cable to establish pairing)
- [ ] Connect external trackpad (use physical cable to establish pairing)
- Displays
  - [ ] Connect external displays to laptop
  - [ ] Arrange external displays
  - [ ] Set scaling on any Retina displays
- [ ] Pair headphones with laptop
- [ ] Install latest updates
- [ ] Reboot
- [ ] Install [brew](https://brew.sh/)

  ```bash
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
  ```

- [ ] Clone this repository

  ```bash
  mkdir -p src
  cd src
  git clone https://github.com/jml/dotfiles.git
  cd dotfiles
  ```

- [ ] Install packages

  ```bash
  cd macos
  brew bundle
  cd ..
  ```

- [ ] **Run the automated system configuration script**

  ```bash
  ./macos-setup.sh
  ```

  This script automates most of the system configuration steps. For manual alternatives to any automated step, see the comments in the script file.

- [ ] Create an SSH key and register it with GitHub

  ```bash
  ssh-keygen -t ed25519
  ```

- [ ] Load the key into memory

  ```bash
  ssh-add --apple-use-keychain
  ```

- [ ] Change the clone to use SSH

  ```bash
  git remote set-url origin git@github.com:jml/dotfiles.git
  ```

- [ ] Change iTerm2 so that left option is Esc +
- [ ] Change iTerm2 font to Source Code Pro
- [ ] Install oh-my-zsh
  - [ ] `sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"`
  - [ ] Install spaceship prompt: https://github.com/denysdovhan/spaceship-prompt
        https://github.com/denysdovhan/spaceship-prompt#oh-my-zsh
- [ ] Install iTerm2 Shell Integration

- [ ] Install configurations

  ```bash
  just all
  ```
- [ ] Launch Emacs
- Omnifocus
  - [ ] Run Omnifocus and log in with OmniFocus account
  - [ ] Link personal calendar to laptop
  - [ ] Link work calendar to laptop
  - [ ] Configure Forecast view to use `Time : Today` tag
  - [ ] Configure Forecast view to show work & personal calendars
  - Star commonly used perspectives
    - [ ] Easy wins
    - [ ] Weekly review
- [ ] Set up Google Backup and Sync to get my own folders
- [ ] Trim useless applications from Dock and make sure frequently used ones are there:
  - [ ] Emacs
  - [ ] iTerm2
  - [ ] OmniFocus
- Set up Xcode
  - [ ] Launch Xcode (do this online)
  - [ ] Set location of command-line tools (Preferences > Locations)
  - [ ] Install Xcode Command Line Tools

    ```bash
    xcode-select --install
    ```

Tip: Ctrl-F7 toggles "Tab to get everywhere". Useful workaround for when "Allow" button not working.

#### UK (ISO) keyboard layout?

- [ ] Set up Karabiner-Elements to swap stupid UK key (`non_us_backslash`) for tilde (`grave_accent_and_tilde`)

TODO: Various language servers for Emacs

#### Aerospace

https://nikitabobko.github.io/AeroSpace/guide#tree

Essentially equivalent of i3 for macOS. Configuration is installed via `just all`.

#### Sketchybar

https://felixkratz.github.io/SketchyBar/setup

```
brew tap FelixKratz/formulae
brew install sketchybar
brew services start sketchybar
```

## Automation References

The `macos-setup.sh` script automates many of the manual configuration steps above. The following resources were particularly helpful for finding the command-line alternatives:

### Key Resources

- **[mathiasbynens/dotfiles](https://github.com/mathiasbynens/dotfiles)** - One of the most comprehensive collections of macOS automation commands with detailed explanations
- **[Awesome macOS Command Line](https://github.com/herrbischoff/awesome-macos-command-line)** - Curated list of useful command-line tools and system configuration commands for macOS
- **[Apple Technical Documentation](https://developer.apple.com/documentation/)** - Official documentation for understanding preference files (`.plist`) and system domains
- **[Ask Different (Apple Stack Exchange)](https://apple.stackexchange.com/)** - Community Q&A for specific macOS configuration challenges
- **[Stack Overflow](https://stackoverflow.com/)** - Technical solutions for command-line automation and edge cases

### Essential Command Documentation

- `man defaults` - Primary tool for modifying system preferences programmatically  
- `man scutil` - System configuration utility for network and system settings
- `man softwareupdate` - Software update configuration and management
- `man hidutil` - Hardware input device utility for keyboard remapping

### Notes

Apple frequently changes preference keys and domains between macOS versions, so commands may need updates for newer releases. The GitHub dotfiles repositories listed above are actively maintained and provide the most current working solutions.
