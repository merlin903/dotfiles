#!/usr/bin/env bash
# NOTE: This file is generated from $HOME/Development/sametjan/dotfiles/Systems.org. Please see commentary there.
# Close any open System Preferences panes, to prevent them from overriding
# settings we're about to change.
osascript -e 'tell application "System Preferences" to quit'

# Ask for the administrator password upfront
sudo -v

# Keep-alilve: update existing `sudo` timestamp until script has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

sudo nvram SystemMute=%00

# Use dark mode
defaults write NSGlobalDomain AppleInterfaceStyle -string Dark

# Set accent color to Graphite
defaults write NSGlobalDomain AppleAccentColor -int -1

# Set highlight color to Graphite
defaults write NSGlobalDomain AppleHighlightColor -string "0.847059 0.847059 0.862745"

# Automatically show/hide the scrollbar
defaults write NSGlobalDomain AppleShowScrollBars -string "Automatic"

# Disable the focus ring animation
defaults write NSGlobalDomain NSUseANimatedFocusRing -bool false

# Expand the save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Expand the print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Automatically quit the printer app when the print jobs complete
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# Disable Resume system-wide
defaults write com.apple.systempreferences NSQuitAlwaysKeepsWindows -bool false

# Disable automatic termination of inactive apps
defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true

# Reveal IP address, hostname, OS version, etc. when clicking the clock
# in the login window
sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

# Trackpad: enable tap to click for this user and for the login screen
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Enable full keyboard access for all controls
# (e.g. enable Tab in modal dialogs)
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# Use scroll gesture with the Ctrl (^) modifier key to zoom
defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true
defaults write com.apple.universalaccess HIDScrollZoomModifierMask -int 262144
# Follow the keyboard focus while zoomed in
defaults write com.apple.universalaccess closeViewZoomFollowsFocus -bool true

# Set language and text formats
# US English and Japanese
defaults write NSGlobalDomain AppleLanguages -array "en-US" "ja"
defaults write NSGlobalDomain AppleLocale -string "en_US@currency=USD"
defaults write NSGlobalDomain AppleMeasurementUnits -string "Inches"
defaults write NSGlobalDomain AppleMetricUnits -bool false

# Show language menu in the top right corner of the boot screen
sudo defaults write /Library/Preferences/com.apple.loginwindow showInputMenu -bool true

# Enable lid wakeup
sudo pmset -a lidwake 1

# Restart automatically on power loss
sudo pmset -a autorestart 1

# Restart automatically if the computer freezes
sudo systemsetup -setrestartfreeze on

# Sleep the display after 15 minutes
sudo pmset -a displaysleep 15

# Disable machine sleep while charging
sudo pmset -c sleep 0

# Set machine sleep to 5 minutes on battery
sudo pmset -b sleep 5

# Set standby delay to 24 hours (default is 1 hour)
sudo pmset -a standbydelay 86400

# Never go into computer sleep mode
sudo systemsetup -setcomputersleep Off > /dev/null

# Hibernation mode
# 0: Disable hibernation (speeds up entering sleep mode)
# 3: Copy RAM to disk so the system state can still be restored in case of a
#    power failure.
sudo pmset -a hibernatemode 0

# Remove the sleep image file to save disk space
sudo rm /private/var/vm/sleepimage
# Create a zero-byte file instead…
sudo touch /private/var/vm/sleepimage
# …and make sure it can’t be rewritten
sudo chflags uchg /private/var/vm/sleepimage

################################################################
# Kill all affected applications
################################################################
for app in "Activity Monitor" \
	       "Address Book" \
	       "Calendar" \
	       "cfprefsd" \
	       "Contacts" \
	       "Dock" \
	       "Finder" \
	       "Google Chrome" \
	       "Mail" \
	       "Messages" \
	       "Photos" \
	       "Safari" \
	       "SystemUIServer" \
	       "Terminal"; do
    killall "${app}" &> /dev/null
done

echo "Done. Note that some of these changes require a logout/restart to take effect."

# Get computer name
currentName=$(scutil --get ComputerName)

computerName="Soundwave"
# Set computer name (as done via System Preferences -> Sharing)
if [[ $currentName =~ (MacBook Pro)$ ]]; then
    sudo scutil --set ComputerName $computerName
    sudo scutil --set HostName  ${computerName// /-}
    sudo scutil --set LocalHostName ${computerName// /-}
fi
