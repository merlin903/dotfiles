#########################
# Autojump
#########################
[ -f /usr/local/share/autojump/autojump.fish ]; and source /usr/local/share/autojump/autojump.fish

#########################
# PyEnv
#########################
# Load pyenv automatically by appending
# the following to ~/.config/fish/config.fish:
status --is-interactive; and source (pyenv init -|psub)

# Load pyenv-virtualenv automatically by adding
# the following to ~/.config/fish/config.fish:
status --is-interactive; and source (pyenv virtualenv-init -|psub)
