# Source aliases
[ -f "$ZDOTDIR/alias" ] && source "$ZDOTDIR/alias"

# Load modules
zmodload zsh/complist
autoload -U compinit && compinit
autoload -U colors && colors

# Completion
zstyle ':completion:*' menu select # tab opens cmp menu
zstyle ':completion:*' special-dirs false # force . and .. to show in cmp menu
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} ma=0\;33 # colorize cmp menu
zstyle ':completion:*' squeeze-slashes false # explicit disable to allow /*/ expansion

# Options
setopt append_history inc_append_history share_history # better history
# on exit, history appends rather than overwrites; history is appended as soon as cmds executed; history shared across sessions
setopt auto_menu menu_complete # autocmp first menu match
setopt autocd # type a dir to cd
setopt auto_param_slash # when a dir is completed, add a / instead of a trailing space
setopt no_case_glob no_case_match # make cmp case insensitive
setopt globdots # include dotfiles
setopt extended_glob # match ~ # ^
setopt interactive_comments # allow comments in shell
unsetopt prompt_sp # don't autoclean blanklines
stty stop undef # disable accidental ctrl s

# History opts
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE="$XDG_CACHE_HOME/zsh_history" # move histfile to cache
HISTCONTROL=ignoreboth # consecutive duplicates & commands starting with space are not saved

# Prompt style
PROMPT="${NEWLINE}%K{#2E3440}%F{#E5E9F0}$(date +%_H:%M%P) %K{#3b4252}%F{#ECEFF4} %n %K{#4c566a} %~ %f%k ${NEWLINE}%F{green} ❯%F{reset_color} "
echo -e "${NEWLINE}\033[48;2;46;52;64;38;2;216;222;233m $0 \033[0m\033[48;2;59;66;82;38;2;216;222;233m $(uptime -p | cut -c 4-) \033[0m\033[48;2;76;86;106;38;2;216;222;233m $(uname -r) \033[0m"

# Set cursor style to
# non-blinking vertical line
echo '\e[6 q'

# Delete to previous slash,
# useful for editing paths
autoload -U select-word-style
select-word-style bash

# Emacs keybindings
set -o emacs

# Copy to system clipboard
x-copy-region-as-kill () {
  zle copy-region-as-kill
  print -rn -- $CUTBUFFER | xsel -i -b
}

zle -N x-copy-region-as-kill
x-kill-region () {
  zle kill-region
  print -rn -- $CUTBUFFER | xsel -i -b
}

zle -N x-kill-region
x-yank () {
  CUTBUFFER=$(xsel -o -b </dev/null)
  zle yank
}

zle -N x-yank

bindkey -e '\ew' x-copy-region-as-kill
bindkey -e '^W' x-kill-region
bindkey -e '^Y' x-yank

# View man pages in Emacs
function man () {
    emacsclient -nw -e '(man "'$1'")'
}

#
# Plugins
#

# Set up fzf keybindings and fuzzy completion
source /usr/share/doc/fzf/examples/key-bindings.zsh
source /usr/share/doc/fzf/examples/completion.zsh
