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

# Custom zsh files directory location
export ZSH_CUSTOM="$HOME/dotfiles/zsh/custom"

# Path
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
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
elif [[ "$OSTYPE" == "darwin"* ]]; then
    path=(
        /opt/local/sbin
        /opt/local/bin
        /usr/local/bin
        /usr/bin
        /Users/najjt/Library/Python/3.9/bin
        /opt/homebrew/bin:/opt/homebrew/sbin
        /Library/TeX/texbin
        $path
    )
fi

export PATH="$PATH"

# Jupyter
export JUPYTER_PATH=/opt/homebrew/share/jupyter
export JUPYTER_CONFIG_PATH=/opt/homebrew/etc/jupyter

# Java
export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_202.jdk/Contents/Home"
#export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk-21.jdk/Contents/Home"
