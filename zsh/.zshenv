# XDG Environment variables
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# Language & locale
export LANGUAGE="en_US.UTF-8"
export LANG="en_US.UTF-8"
export LC_ALL="sv_SE.UTF-8"
export LC_CTYPE="sv_SE.UTF-8"

# Default programs
export BROWSER=qutebrowser
export TERMINAL=alacritty
export EDITOR=emacsclient
export PAGER=less
export GPG_TTY=$(tty)

export GTK_THEME=Adwaita-dark

# Path
path=(
    /usr/local/sbin
    /usr/local/bin
    /usr/sbin
    /usr/bin
    /sbin
    /bin
    /home/najjt/.local/bin
    /home/najjt/go/bin
)

export PATH="$PATH"

# Java
export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")
