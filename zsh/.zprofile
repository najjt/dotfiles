# XDG Environment variables
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# Disable zsh session files being written
export SHELL_SESSIONS_DISABLE=1

# Defaults
export EDITOR=emacsclient
export TERM=xterm-256color
export TERMINAL=kitty
export BROWSER=qutebrowser
# export DISPLAY=:0 # useful for some scripts
export GPG_TTY=$(tty)
export GTK_THEME=Adwaita-dark

# Language & locale
export LANGUAGE="en_IE.UTF-8"
export LANG="en_IE.UTF-8"
export LC_ALL="en_IE.UTF-8"
export LC_CTYPE="en_IE.UTF-8"

# Path
export PATH="$PATH:$HOME/.local/bin:$HOME/go/bin:/usr/local/go/bin:$HOME/scripts"

# bootstrap .zshrc to ~/.config/zsh/.zshrc, any other zsh config files can also reside here
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Java
export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")

# Cargo
. "$HOME/.cargo/env"

export $(dbus-launch)
