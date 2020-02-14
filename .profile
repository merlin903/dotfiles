#!/usr/bin/env bash

export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.ghcup/bin:/$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# PyEnv
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# RbEnv
if command -v rbenv 1>/dev/null 2>&1; then
  eval "$(rbenv init -)"
fi

# The Fuck
eval $(thefuck --alias)
. /usr/local/ets/profile.d/z.sh
