# Systemd services
alias ecre="systemctl --user restart emacs"     # Restart Emacs service
alias stre="systemctl --user restart syncthing" # Restart Syncthing service

alias dnf="sudo dnf"

alias vpn-up="wg-quick up sto11-adb"
alias vpn-down="wg-quick down sto11-adb"

# Prompt theme
source ~/programs/powerlevel10k/powerlevel10k.zsh-theme
