# XDG Environment variables
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_DATA_DIRS="$HOME/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:/usr/local/share/:/usr/share/"

# Disable zsh session files being written
export SHELL_SESSIONS_DISABLE=1

# Defaults
export EDITOR=emacsclient
export TERM=xterm-256color
export GPG_TTY=$(tty)

# Language & locale
export LANGUAGE="en_IE.UTF-8"
export LANG="en_IE.UTF-8"
export LC_ALL="en_IE.UTF-8"
export LC_CTYPE="en_IE.UTF-8"

# Path
export PATH="$HOME/.local/bin:$HOME/scripts:$PATH"

# Java
export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")

# fzf default options
export FZF_DEFAULT_OPTS="--color 16 --layout=reverse --height 30% --preview='batcat -p --color=always {}'"
export FZF_CTRL_R_OPTS="--color 16 --info inline --no-sort --no-preview" # Separate opts for history widget

# Additional completions
fpath=(path/to/zsh-completions/src $fpath)

export $(dbus-launch)
