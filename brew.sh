#!/bin/bash

# Install Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install command line tools using Homebrew

# Make sure we're using the latest Homebrew
brew update

# Upgrade any already-installed formulae
brew upgrade

# GNU core utilities (those that come with OSX are outdated)
brew install coreutils
brew install moreutils
# GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed
brew install findutils
# GNU `sed`, overwriting the built-in `sed`
brew install gnu-sed --with-default-names


# Bash 4
# Note: don't forget to add `/usr/local/bin/bash` to `/etc/shells` before
# running `chsh`
brew install bash
brew install bash-completion
brew install brew-cask-completion

# ZSH
# Note: don't forget to add `/usr/local/zsh` to `/etc/shells` before
# running `chsh`
brew install zsh

# Fish
# Note: don't forget to add `/usr/local/fish` to `/etc/shells` before
# running `chsh`
brew install fish

# Install wget with IRI support
brew install wget --with-iri

# Install more recent versions of some OSX tools
brew install vim --with-override-system-vi
brew install nano
brew install grep
brew install screen

# z hopping around folders
brew install z

# Run this script when this file changes guy
brew install entr

# Github util
brew install hub

# mtr - ping & traceroute
brew install mtr

    # allow mtr to run without sudo
    # e.g. `/usr/local/Cellar/mtr/0.92`
    mtrlocation=$(brew info mtr | grep Cellar | sed -e 's/ (.*//')
    sudo chmod 4755 $mtrlocation/sbin/mtr
    sudo chown root $mtrlocation/sbin/mtr

# Install other useful binaries
brew install the_silver_searcher
brew install fzf

brew install git
brew install imagemagick --with-webp
brew install pv
brew install rename
brew install tree
brew install zopfli
brew install ffmpeg --with-libvpx
brew install unar

# Install node with nvm
brew install yarn --without-node

# Install newer python and python3
brew install python
brew install python3

# Heroku for deploying
brew install heroku

brew install terminal-notifier
brew install highlight
brew install thefuck

# brew install android-platform-tools
brew install pidcat # colored logcat guy

brew install ncdu # find where your diskspace went

# MAS - Install/update apps from the App Store
brew install mas

brew install ical-buddy

# Remove outdated versions from the cellar
brew cleanup
