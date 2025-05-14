# dotfiles

jml's configuration

## Howto

### macOS

- [ ] Connect to the Internet
- [ ] Get Apple ID credentials from 1Password on phone to sign in
- Keyboard
  - [ ] Set keyboard layout to Dvorak
  - [ ] Change Caps Lock to be Control on laptop keyboard (Settings > Keyboard > Modifier Keys)
  - [ ] Connect external keyboard (use physical cable to establish pairing)
  - [ ] Change Caps Lock to be Control on external keyboard (Settings > Keyboard > Modifier Keys)
- Trackpad
  - [ ] Connect external trackpad (use physical cable to establish pairing)
  - [ ] Set trackpad to "Tap to Click" (Settings > Trackpad)
- Displays
  - [ ] Connect external displays to laptop
  - [ ] Arrange external displays
  - [ ] Set scaling on any Retina displays
- [ ] Pair headphones with laptop
- [ ] Configure system to automatically update
- [ ] Install latest updates
- [ ] Reboot
- [ ] Configure Dock to auto-hide
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

- [ ] Set host name with System Preferences > Sharing > Computer Name
- [ ] System Preferences > Security & Privacy > General > Require password 5 seconds after sleep
- [ ] System Preferences > Desktop & Screen Saver > Screen Saver > Start after 5 minutes

- [ ] Install Emacs configuration

  ```bash
  ln -s ~/src/dotfiles/emacs ~/.emacs.d
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
- [ ] Set Chrome as default browser
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

#### If someone else chose the system language

Change your keyboard layout

- [ ] Log in.
- [ ] From the Apple menu, choose System Preferences.
- [ ] Click the Users & Groups icon.
- [ ] Click the lock icon. Enter an admin user name and password.
- [ ] Click Login Options.
- [ ] Select the option to Show Input menu in login window.

Source: https://support.apple.com/en-gb/HT202036

TODO: Various language servers for Emacs

