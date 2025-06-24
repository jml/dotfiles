#!/bin/bash

# macOS System Configuration Automation Script
# This script automates many of the manual configuration steps from the README

set -e

echo "🍎 Starting macOS system configuration..."

# Function to check if we're running on macOS
check_macos() {
    if [[ "$OSTYPE" != "darwin"* ]]; then
        echo "❌ This script only works on macOS"
        exit 1
    fi
}

# Function to prompt for user confirmation
confirm() {
    read -p "$1 (y/N): " -n 1 -r
    echo
    [[ $REPLY =~ ^[Yy]$ ]]
}

# Function to get keyboard vendor/product IDs for Caps Lock remapping
get_keyboard_info() {
    echo "🔍 Finding keyboard information for Caps Lock remapping..."
    ioreg -p IOUSB -c IOUSBDevice | grep -e "class IOUSBDevice" -e idVendor -e idProduct | head -20
    echo "📝 Note: You may need to manually configure Caps Lock in System Preferences"
}

check_macos

echo "This script will configure various macOS system settings."
echo "Some changes require administrator privileges and system restarts."
echo ""

if ! confirm "Do you want to proceed with the configuration?"; then
    echo "Aborted."
    exit 0
fi

# 1. Configure Dock to auto-hide
# Manual equivalent: System Preferences > Dock & Menu Bar > Automatically hide and show the Dock
echo "🖥️  Configuring Dock to auto-hide..."
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-delay -int 0
defaults write com.apple.dock autohide-time-modifier -float 0.4
killall Dock

# 2. Enable trackpad "Tap to Click"
# Manual equivalent: System Preferences > Trackpad > Point & Click > Tap to click
echo "👆 Enabling trackpad 'Tap to Click'..."
defaults write com.apple.AppleMultitouchTrackpad Clicking -bool true
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Apply trackpad settings immediately
if command -v /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings >/dev/null 2>&1; then
    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
fi

# 3. Configure automatic software updates
# Manual equivalent: System Preferences > Software Update > Advanced > Check for updates, Download new updates, Install macOS updates, Install app updates, Install system data files and security updates
echo "🔄 Configuring automatic software updates..."
sudo softwareupdate --schedule on
sudo defaults write /Library/Preferences/com.apple.SoftwareUpdate.plist AutomaticCheckEnabled -bool YES
sudo defaults write /Library/Preferences/com.apple.SoftwareUpdate.plist AutomaticDownload -bool YES
sudo defaults write /Library/Preferences/com.apple.SoftwareUpdate.plist AutomaticallyInstallMacOSUpdates -bool YES
sudo defaults write /Library/Preferences/com.apple.SoftwareUpdate.plist ConfigDataInstall -bool YES
sudo defaults write /Library/Preferences/com.apple.SoftwareUpdate.plist CriticalUpdateInstall -bool YES

# 4. Set screen saver to start after 5 minutes
# Manual equivalent: System Preferences > Desktop & Screen Saver > Screen Saver > Start after: 5 Minutes
echo "🔒 Setting screen saver to start after 5 minutes..."
defaults -currentHost write com.apple.screensaver idleTime -int 300

# 5. Configure screen lock password requirement (may not work on newer macOS)
# Manual equivalent: System Preferences > Security & Privacy > General > Require password 5 seconds after sleep or screen saver begins
echo "🔐 Attempting to configure screen lock password requirement..."
defaults write com.apple.screensaver askForPassword -bool true
defaults write com.apple.screensaver askForPasswordDelay -int 5

# 6. Show input menu in login window
# Manual equivalent: System Preferences > Users & Groups > Login Options > Show Input menu in login window
echo "⌨️  Enabling input menu in login window..."
sudo defaults write /Library/Preferences/com.apple.loginwindow showInputMenu -bool TRUE

# 7. Caps Lock to Control key remapping setup
# Manual equivalent: System Preferences > Keyboard > Modifier Keys > Caps Lock Key: Control
echo "🔧 Setting up Caps Lock to Control remapping..."
get_keyboard_info

# Attempt to set up Caps Lock remapping using hidutil (temporary method)
echo "Attempting to remap Caps Lock to Control (temporary)..."
hidutil property --set '{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000039,"HIDKeyboardModifierMappingDst":0x7000000E0}]}'

# 8. Set hostname
# Manual equivalent: System Preferences > Sharing > Computer Name
if confirm "Do you want to set a custom hostname?"; then
    read -p "Enter hostname: " hostname
    if [ -n "$hostname" ]; then
        echo "🏷️  Setting hostname to '$hostname'..."
        sudo scutil --set HostName "$hostname"
        sudo scutil --set LocalHostName "$hostname"
        sudo scutil --set ComputerName "$hostname"
        dscacheutil -flushcache
    fi
fi

# 9. Set Chrome as default browser (if installed)
# Manual equivalent: System Preferences > General > Default web browser > Google Chrome
# Or: Chrome > Preferences > Default browser > Make default
if [ -d "/Applications/Google Chrome.app" ]; then
    if confirm "Set Chrome as default browser?"; then
        echo "🌐 Setting Chrome as default browser..."
        # This will prompt the user for confirmation
        open -a "Google Chrome" --args --make-default-browser
    fi
fi

# 10. Add Dvorak keyboard layout (requires manual selection afterward)
# Manual equivalent: System Preferences > Keyboard > Input Sources > + > English > Dvorak
if confirm "Add Dvorak keyboard layout? (requires manual selection in System Preferences)"; then
    echo "⌨️  Adding Dvorak keyboard layout..."
    defaults write com.apple.HIToolbox AppleEnabledInputSources -array-add '<dict><key>InputSourceKind</key><string>Keyboard Layout</string><key>KeyboardLayout ID</key><integer>16300</integer><key>KeyboardLayout Name</key><string>Dvorak</string></dict>'
    echo "📝 Note: Please manually select Dvorak in System Preferences > Keyboard > Input Sources"
fi

echo ""
echo "✅ Configuration complete!"
echo ""
echo "📋 Manual steps still required:"
echo "   • Set Caps Lock to Control in System Preferences > Keyboard > Modifier Keys"
echo "   • Configure iTerm2 settings (left option key, Source Code Pro font)"
echo "   • Select Dvorak keyboard layout in System Preferences (if added)"
echo "   • Some settings may require logging out and back in to take effect"
echo ""
echo "🔄 Consider restarting your system to ensure all changes take effect."