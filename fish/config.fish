set default_user "sametjan"
set default_machine "Soundwave"

set EDITOR "emacsclient"

set -gx SSL_CERT_FILE /etc/ssl/cert.pem

source ~/.config/fish/path.fish
source ~/.config/fish/aliases.fish
source ~/.config/fish/chpwd.fish
source ~/.config/fish/functions.fish
source ~/.config/fish/plugins.fish
source ~/.config/fish/sources.fish
source ~/.config/fish/iterm/fish_startup.in

# For things not checked into git
if test -e "$HOME/.extra.fish"
   source ~/.extra.fish
end

# Completions
function make_completion --argument-names alias command
    echo "
    function __alias_completion_$alias
        set -l cmd (commandline -o)
	set -e cmd[1]
	complete -C\"$command \$cmd\"
    end
    " | .
    complete -c $alias -a "(__alias_completion_$alias)"
end

make_completion g 'git'
