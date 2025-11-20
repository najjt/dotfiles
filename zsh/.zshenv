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
export TERMINAL=kitty
export BROWSER=qutebrowser
export GPG_TTY=$(tty)
export GTK_THEME=Adwaita:dark

# Language & locale
export LANGUAGE="en_IE.UTF-8"
export LANG="en_IE.UTF-8"
export LC_ALL="en_IE.UTF-8"
export LC_CTYPE="en_IE.UTF-8"

# ~/ cleanup
export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
export ZDOTDIR="$XDG_CONFIG_HOME"/zsh
export GTK_RC_FILES="$XDG_CONFIG_HOME"/gtk-1.0/gtkrc
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export STACK_ROOT="$XDG_DATA_HOME"/stack
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export ICEAUTHORITY="$XDG_CACHE_HOME"/ICEauthority
export LESSHISTFILE="-"

# Path
export PATH="$CARGO_HOME/bin:$XDG_DATA_HOME/nix/profile/bin:/nix/var/nix/profiles/default/bin:$HOME/.local/bin:$HOME/scripts:$PATH"

# Java
export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")

# fzf default options
export FZF_DEFAULT_OPTS="--color 16 --layout=reverse --height 30% --preview='batcat -p --color=always {}'"
export FZF_CTRL_R_OPTS="--color 16 --info inline --no-sort --no-preview" # Separate opts for history widget

export $(dbus-launch)
