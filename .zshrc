# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall

zstyle ':completion:*' auto-description '`specify: %d'\'''
zstyle ':completion:*' completer _expand _complete _correct
zstyle ':completion:*' file-sort name
zstyle ':completion:*' format ''\''Completing %d'\'''
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous false
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false

zstyle :compinstall filename '$HOME/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

namedir () { $1=$PWD ;  : ~$1 }

bindkey -e

#To discover what keycode is being sent, hit ^v
#and then the key you want to test.

bindkey '^R' history-incremental-search-backward

bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[3~" delete-char
bindkey "\e[2~" quoted-insert
bindkey "\e[5C" forward-word
bindkey "\eOc" emacs-forward-word
bindkey "\e[5D" backward-word
bindkey "\eOd" emacs-backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
# for non RH/Debian xterm, can't hurt for RH/Debian xterm
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line
# for freebsd console
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line


# colors
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    (( count = $count + 1 ))
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"

PROMPT="${PR_WHITE}%n${PR_NO_COLOR}@%m ${PR_GREEN}%~ ${PR_BLUE}%%${PR_NO_COLOR} "

export LESS="FSRX"
alias ls="ls --color"
alias grep="grep --color"

setopt extendedglob
setopt autocd
setopt correctall

export DEBFULLNAME="Renzo Carbonara"
export DEBEMAIL="gnuk0001@gmail.com"

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

export EDITOR=vim

export TERM=xterm-256color

#Colores para el grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;31'

export PIP_REQUIRE_VIRTUALENV=1
export PIP_RESPECT_VIRTUALENV=1

export PYTHONSTARTUP=$HOME/.pythonrc.py

# This loads RVM into a shell session.
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

export PATH="$HOME/bin:$PATH"

export NODE_PATH="$HOME/.node_libraries:$NODE_PATH"
