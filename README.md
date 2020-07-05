# dotfiles

jml's configuration

## Howto

### macOS

- [x] Connect to the Internet
- [x] Get Apple ID credentials from 1Password on phone to sign in
- Keyboard
  - [x] Set keyboard layout to Dvorak
  - [x] Change Caps Lock to be Control on laptop keyboard (Settings > Keyboard > Modifier Keys)
  - [ ] Connect external keyboard (use physical cable to establish pairing)
  - [ ] Change Caps Lock to be Control on external keyboard (Settings > Keyboard > Modifier Keys)
- Trackpad
  - [ ] Connect external trackpad (use physical cable to establish pairing)
  - [x] Set trackpad to "Tap to Click" (Settings > Trackpad)
- Displays
  - [ ] Connect external displays to laptop
  - [ ] Arrange external displays
  - [ ] Set scaling on any Retina displays
- [ ] Pair headphones with laptop
- [x] Configure system to automatically update
- [x] Install latest updates
- [x] Reboot
- [x] Configure Dock to auto-hide
- [x] Install [brew](https://brew.sh/)

  ```bash
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
  ```

- [x] Clone this repository

  ```bash
  mkdir -p src
  cd src
  git clone https://github.com/jml/dotfiles.git
  cd dotfiles
  ```

- [x] Install packages

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

- [x] Change the clone to use SSH

  ```bash
  git remote set-url origin git@github.com:jml/dotfiles.git
  ```

- [x] Change iTerm2 so that left option is Esc +
- [x] Change iTerm2 font to Source Code Pro
- [ ] Install oh-my-zsh
  - [ ] `sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"`
  - [ ] Install spaceship prompt: https://github.com/denysdovhan/spaceship-prompt
        https://github.com/denysdovhan/spaceship-prompt#oh-my-zsh
- [x] Install iTerm2 Shell Integration

- Set host name
  - [x] System Preferences > Sharing > Computer Name
  - [x] Terminal

    ```console
    sudo scutil --set <HostName>
    ```

- [x] System Preferences > Security & Privacy > General > Require password 5 seconds after sleep
- [x] System Preferences > Desktop & Screen Saver > Screen Saver > Start after 5 minutes

- [x] Install Emacs configuration

  ```bash
  ln -s ~/src/dotfiles/emacs ~/.emacs.d
  ```  
- [x] Launch Emacs
- [x] Give Moom access to the system
- Change Moom settings
  - [x] Always launch
  - [x] Not show preferences on launch
  - [x] Run as menu bar application
  - [x] Add Opt+Arrow to move between displays
  - [x] Set Cmd+F4 as Moom hotkey
- Omnifocus
  - [x] Run Omnifocus and supply license (kept in 1Password)
  - [x] Link personal calendar to laptop
  - [x] Link work calendar to laptop
  - [x] Configure Forecast view to use `Time : Today` tag
  - [x] Configure Forecast view to show work & personal calendars
  - Star commonly used perspectives
    - [x] Easy wins
    - [x] Weekly review
    - [x] Solo
    - [x] Tube
- Fantastical
- [x] Set up Google Backup and Sync to get my own folders
- [x] Run Dash and provide license file (`~/Google Drive/Documents/Dash/dash.license`)
- [x] Set Chrome as default browser
- [x] Trim useless applications from Dock and make sure frequently used ones are there:
  - [x] Emacs
  - [x] iTerm2
  - [x] OmniFocus
- Set up Xcode
  - [x] Launch Xcode (do this online)
  - [x] Set location of command-line tools (Preferences > Locations)
  - [x] Install Xcode Command Line Tools

    ```bash
    xcode-select --install
    ```

Tip: Ctrl-F7 toggles "Tab to get everywhere". Useful workaround for when "Allow" button not working.

#### UK (ISO) keyboard layout?

- [x] Set up Karabiner-Elements to swap stupid UK key (`non_us_backslash`) for tilde (`grave_accent_and_tilde`)

#### Memrise-specific stuff

- [x] Set up Google Drive File Stream for work folders
- [ ] Get Pritunl configured to connect to VPN


#### If someone else chose the system language

Change your keyboard layout

- [x] Log in.
- [x] From the Apple menu, choose System Preferences.
- [x] Click the Users & Groups icon.
- [x] Click the lock icon. Enter an admin user name and password.
- [x] Click Login Options.
- [x] Select the option to Show Input menu in login window.

Source: https://support.apple.com/en-gb/HT202036

TODO: Various language servers for Emacs

