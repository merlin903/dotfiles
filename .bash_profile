#!/usr/bin/env bash

# Bash Completion
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# Autojump
[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh

# Load shared profile
[ -f $HOME/.profile ] && . $HOME/.profile
if [ -e /Users/steven.ametjan/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/steven.ametjan/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
