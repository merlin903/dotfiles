#!/bin/sh
# NOTE: This file is generated from ~/Development/sametjan/dotfiles/System.org.
# Install Homebrew
/usr/bing/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew tap d12frosted/emacs-plus # Better Emacs
brew tap homebrew/cask # MacOS apps
brew tap homebrew/cask-fonts # Fonts
brew tap homebrew/cask-versions # Alternate versions

brew install coreutils
brew install moreutils
brew install findutils
brew install gnu-sed

brew install bash
brew install bash-completion

brew install zsh
brew install fish

echo $(brew --prefix)/bin/bash | sudo tee -a /etc/shells
echo $(brew --prefix)/bin/zsh | sudo tee -a /etc/shells
echo $(brew --prefix)/bin/fish | sudo tee -a /etc/shells
