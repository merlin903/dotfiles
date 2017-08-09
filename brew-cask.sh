#!/bin/bash

# Install Cask
brew tap caskroom/cask
brew tap caskroom/versions

# Utilities
brew cask install astropad
brew cask install bartender
brew cask install dropbox
brew cask install gyazo
brew cask install quicksilver
brew cask install skyfonts
brew cask install spamsieve
brew cask install the-unarchiver

# Development
brew cask install docker
brew cask install imagealpha
brew cask install imageoptim
brew cask install iterm2
brew cask install sketch
brew cask install sketch-toolbox
brew cask install visual-studio-code-insiders

# Browsers
brew cask install firefox
brew cask install google-chrome-beta
brew cask install safari-technology-preview

# Screensaver
brew cask install fliqlo

# Media
brew cask install bowtie
brew cask install plex-media-player
brew cask install plex-media-server
brew cask install sonarr-menu
brew cask install spotify
brew cask install vlc

# Other
brew cask install adium-beta
brew cask install google-hangouts
brew cask install growlnotify
brew cask install hazel
brew cask install java
brew cask install jdownloader
brew cask install openemu
brew cask install steam
brew cask install transmission
brew cask install ubersicht


# Quicklook

# Set qlplugindir
export HOMEBREW_CASK_OPTS='--qlplugindir=/Library/QuickLook/'

brew cask install betterzipql
brew cask install qlcolorcode
brew cask install qlimagesize
brew cask install qlmarkdown
brew cask install qlprettypatch
brew cask install qlstephen
brew cask install qlvideo
brew cask install quicklook-csv
brew cask install quicklook-json
brew cask install quicklookase
brew cask install quicknfo
brew cask install suspicious-package
brew cask install webpquicklook
