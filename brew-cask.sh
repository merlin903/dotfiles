#!/bin/bash

# Install Cask
brew tap homebrew/cask-fonts
brew tap homebrew/cask-versions

# Utilities
brew cask install 1password
brew cask install alfred
brew cask install astropad
brew cask install dropbox
brew cask install gyazo
brew cask install no-ip-duc
brew cask install quicksilver
brew cask install skyfonts
brew cask install spamsieve
brew cask install the-unarchiver
brew cask install tunnelblick

# Commercial Apps
brew cask install adobe-creative-cloud
brew cask install avast-security
brew cask install microsoft-office
brew cask install setapp

# Development
brew cask install arduino
brew cask install docker
brew cask install imagealpha
brew cask install imageoptim
brew cask install iterm2
brew cask install sketch
brew cask install sketch-toolbox
brew cask install visual-studio-code-insiders

# Browsers
brew cask install firefox-developer-edition
brew cask install google-chrome-beta
brew cask install safari-technology-preview

# Screensaver
brew cask install fliqlo
brew cask install aerial

# Media
brew cask install bowtie
brew cask install plex-media-player
brew cask install spotify
brew cask install vlc

# Other
brew cask install discord
brew cask install epic-games
brew cask install growlnotify
brew cask install hazel
brew cask install java
brew cask install jdownloader
brew cask install openemu
brew cask install origin
brew cask install qfinder-pro
brew cask install sony-ps4-remote-play
brew cask install station
brew cask install steam

brew cask install blender
brew cask install meshmixer
brew cask install ultimaker-cura


# Font
brew cask install font-fira-code

# Quicklook

# Set qlplugindir
export HOMEBREW_CASK_OPTS='--qlplugindir=/Library/QuickLook/'

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
