# Set cursor style to
# non-blinking vertical line
echo '\e[6 q'

# Delete to previous slash,
# useful for editing paths
autoload -U select-word-style
select-word-style bash

# History time stamp format
HIST_STAMPS="mm/dd/yyyy"
HISTFILE=~/.zsh_history
SAVEHIST=1000

# Vi mode
bindkey -v

# Fix cursor delay when changing vi modes
KEYTIMEOUT=1

# Set cursor to indicate current vi mode
zle-keymap-select () {
    if [ $KEYMAP = vicmd ]; then
        printf "\033[2 q"
    else
        printf "\033[6 q"
    fi
}
zle -N zle-keymap-select

zle-line-init () {
    zle -K viins
    printf "\033[6 q"
}
zle -N zle-line-init

# Yank to the system clipboard in vi mode
function vi-yank-xclip {
    zle vi-yank
   echo "$CUTBUFFER" | pbcopy -i
}

zle -N vi-yank-xclip
bindkey -M vicmd 'y' vi-yank-xclip

#
#  Aliases
#

# Make aliases work correctly with sudo
alias sudo='sudo '

# If user is not root, pass all commands via sudo
if [ $UID -ne 0 ]; then
    alias reboot='sudo reboot'
    alias shutdown="sudo shutdown -h now"
fi

# Source zsh config
alias sz="source $HOME/.zshrc && source $HOME/.zshenv"

# Navigation
alias ..="cd .."
alias pf="fzf --preview 'bat --color=always {}' --preview-window '~3'"

## Colorize the grep command output for ease of use
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# Create parent directories if necessary,
# and list them as they are created
alias mkdir='mkdir -pv'

# Make mount output be human readable
alias mount='mount | column -t'

# Colorize diff output
alias diff='colordiff'

# Other
alias x="exit"
alias ls='ls -lh --color=always'
alias python="python3"

# Pi
alias pi="ssh pi@192.168.1.48"                             # ssh to raspberry pi
alias pist="ssh -N -L 9090:127.0.0.1:8384 pi@192.168.1.48" # Port forward port 9090 to be able to use syncthing admin gui on other machine

# Emacs
alias e="emacs"
alias ec="emacsclient -c -n -F \"'(fullscreen . maximized)\""

# Systemd services
alias ecre="systemctl --user restart emacs"     # Restart Emacs service
alias stre="systemctl --user restart syncthing" # Restart Syncthing service

#
# Other
#

# View man pages in Emacs
function man () {
    emacsclient -nw -e '(man "'$1'")'
}

#
# Plugins
#

# Set up fzf keybindings and fuzzy completion
source <(fzf --zsh)

# Syntax highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Prompt theme
source ~/programs/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
