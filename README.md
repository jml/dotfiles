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
- [ ] Configure system to automatically update
- [ ] Install latest updates
- [ ] Reboot
- [ ] Configure Dock to auto-hide
- [ ] Install [brew](https://brew.sh/)

  ```bash
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
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
  ssh-add
  ```

- [ ] Change the clone to use SSH

  ```bash
  git remote set-url origin git@github.com:jml/dotfiles.git
  ```

- [ ] Change shell to later version of bash

  ```bash
  sudo vi /etc/shells  # Add /usr/local/bin/bash to shells
  chsh -s /usr/local/bin/bash
  ```

- [ ] Install shell configuration

  ```console
  $ echo '. $HOME/src/dotfiles/bash/bashrc' > ~/.bash_profile
  ```

- [ ] Change iTerm2 so that left option is Esc +

- [ ] Install Emacs configuration

  ```bash
  ln -s ~/src/dotfiles/emacs ~/.emacs.d
  cd ~/.emacs.d/
  cask install
  ```

- [ ] Set up Karabiner-Elements to swap stupid UK key (`non_us_backslash`) for tilde (`grave_accent_and_tilde`)
- [ ] Give Moom access to the system
- Change Moom settings
  - [ ] Always launch
  - [ ] Not show preferences on launch
  - [ ] Run as menu bar application
- [ ] Run Omnifocus and supply license (kept in 1Password)
- [ ] Set up Google Backup and Sync to get my own folders
- [ ] Run Dash and provide license file (`~/Google Drive/Documents/Dash/dash.license`)
- [ ] Set Chrome as default browser
- [ ] Trim useless applications from Dock and make sure frequently used ones are there:
  - [ ] Emacs
  - [ ] iTerm2
  - [ ] OmniFocus
- Set up Xcode
  - [ ] Launch Xcode (do this online)
  - [ ] Set location of command-line tools (Preferences > Locations)

Tip: Ctrl-F7 toggles "Tab to get everywhere". Useful workaround for when "Allow" button not working.


#### Memrise-specific stuff

- [ ] Activate DataGrip license (in 1Password)
- [ ] Set up Google Drive File Stream for work folders
- [ ] Get Pritunl configured to connect to VPN


#### If someone else chose the system language

Change your keyboard layout

- [ ] Log in.
- [ ] From the Apple menu, choose System Preferences.
- [ ] Click the Users & Groups icon.
- [ ] Click the lock icon. Enter an admin user name and password.
- [ ] Click Login Options.
- [ ] Select the option to Show Input menu in login window.

Source: https://support.apple.com/en-gb/HT202036
