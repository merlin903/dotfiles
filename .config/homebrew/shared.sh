#!/usr/bin/env bash
# NOTE: This file is generated from ~/Development/sametjan/dotfiles/System.org.
# It should not be run directly, but as part of the per-system setup script.
# Install Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew tap d12frosted/emacs-plus # Better Emacs
brew tap homebrew/cask # MacOS Apps
brew tap homebrew/cask-fonts # Fonts
brew tap homebrew/cask-versions # Alternate versions from `homebrew/cask`

brew install coreutils
brew install moreutils
brew install findutils
brew install gnu-sed

# Bash
brew install bash
brew install bash-completion

#ZSH and Fish
brew install zsh
brew install fish

# Add them to /etc/shells
echo $(brew --prefix)/bin/bash | sudo tee -a /etc/shells
echo $(brew --prefix)/bin/zsh | sudo tee -a /etc/shells
echo $(brew --prefix)/bin/fish | sudo tee -a /etc/shells

brew install vim # Blasphemy, I know.
brew install grep
brew install git

brew install nvm
brew install pyenv
brew install pyenv-virtualenv
