#!/bin/bash

# Install Cask
brew tap caskroom/cask
brew tap caskroom/versions

# Utilities
brew cask install bartender
brew cask install dropbox
brew cask install gyazo
brew cask install quicksilver
brew cask install skyfonts
brew cask install spamsieve
brew cask install the-unarchiver

# Development
brew cask install iterm2
brew cask install visual-studio-code-insiders
brew cask install imagealpha
brew cask install imageoptim
brew cask install sketch

# Browsers
brew cask install firefox
brew cask install google-chrome-beta
brew cask install safari-technology-preview

# Screensaver
brew cask install fliqlo

# Media
brew cask install plex-media-player
brew cask install plex-media-server
brew cask install sonarr-menu
brew cask install spotify``
brew cask install vlc

# Other
brew cask install adium
brew cask install google-hangouts
brew cask install growlnotify
brew cask install hazel
brew cask install java
brew cask install jdownloader
brew cask install openemu
brew cask install steam
brew cask install transmission

# Quicklook

# Set qlplugindir
export HOMEBREW_CASK_OPTS='--qlplugindir=/Library/QuickLook/'

brew cask install qlcolorcode
brew cask install quicknfo
