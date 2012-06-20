# .zshrc

# ZSH settings
setopt extendedglob
setopt prompt_subst
setopt autocd
setopt correctall
setopt emacs
setopt nohup
setopt cdablevars
setopt nobgnice
setopt nobanghist
setopt clobber
setopt shwordsplit
setopt interactivecomments
setopt autopushd pushdminus pushdsilent pushdtohome
setopt histreduceblanks histignorespace inc_append_history

# ZSH completion
autoload -U compinit; compinit
# List of completers to use
zstyle ":completion:*" completer _expand _complete _match _approximate
# Allow approximate
zstyle ":completion:*:match:*" original only
zstyle ":completion:*:approximate:*" max-errors 1 numeric
# Selection prompt as menu
zstyle ":completion:*" menu select=1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
# Menu selection for PID completion
zstyle ":completion:*:*:kill:*" menu yes select
zstyle ":completion:*:kill:*" force-list always
zstyle ":completion:*:processes" command "ps -au$USER"
zstyle ":completion:*:*:kill:*:processes" list-colors "=(#b) #([0-9]#)*=0=01;32"
# Don't select parent dir on cd
zstyle ":completion:*:cd:*" ignore-parents parent pwd
# Complete with colors
zstyle ":completion:*" list-colors ""

# not sure
zstyle :compinstall filename '$HOME/.zshrc'

compctl -k "(add delete draft edit list import preview publish update)" nb


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


# Prompt stuff

for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"


if [[ "${TERM}" == "dumb" ]]; then
    # Simple prompt for dumb terminals
    unsetopt zle
    PROMPT='%n@%m %l %~ %? %# '
else
    if [[ "${TERM}" == "linux" ]]; then
        # Simple prompt with Zenburn colors for the console
        # These two blacks suck on linux terminals
        #echo -en "\e]P01e2320" # zenburn black (normal black)
        #echo -en "\e]P8709080" # bright-black  (darkgrey)
        echo -en "\e]P1705050" # red           (darkred)
        echo -en "\e]P9dca3a3" # bright-red    (red)
        echo -en "\e]P260b48a" # green         (darkgreen)
        echo -en "\e]PAc3bf9f" # bright-green  (green)
        echo -en "\e]P3dfaf8f" # yellow        (brown)
        echo -en "\e]PBf0dfaf" # bright-yellow (yellow)
        echo -en "\e]P4506070" # blue          (darkblue)
        echo -en "\e]PC94bff3" # bright-blue   (blue)
        echo -en "\e]P5dc8cc3" # purple        (darkmagenta)
        echo -en "\e]PDec93d3" # bright-purple (magenta)
        echo -en "\e]P68cd0d3" # cyan          (darkcyan)
        echo -en "\e]PE93e0e3" # bright-cyan   (cyan)
        echo -en "\e]P7dcdccc" # white         (lightgrey)
        echo -en "\e]PFffffff" # bright-white  (white)
    fi

    # Set the cool prompt!
    if [[ $EUID -ne 0 ]]; then
        PROMPT="${PR_GREEN}%n${PR_NO_COLOR}@${PR_LIGHT_CYAN}%M${PR_NO_COLOR}:%l ${PR_LIGHT_GREEN}%~ ${PR_NO_COLOR}%? ${PR_BLUE}%%${PR_NO_COLOR} "
    else
        PROMPT="${PR_RED}%n${PR_NO_COLOR}@${PR_LIGHT_CYAN}%M${PR_NO_COLOR}:%l ${PR_LIGHT_GREEN}%~ ${PR_NO_COLOR}%? ${PR_RED}%#${PR_NO_COLOR} "
    fi
fi

alias ls="ls --color"
alias grep="grep --color"
alias e="emacsclient -nw -a \"\" -c"




#export HISTFILE=".zsh_history"

export PATH="$HOME/.cabal/bin:$HOME/bin:$HOME/.local/bin:/usr/lib/colorgcc/bin:$PATH"

export EDITOR=$(which vim)

# Colors
export TERM=xterm-256color

export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;31'

export LESS="FSRX"
#export LESS_TERMCAP_mb=$'\E[01;31m'   # begin blinking
#export LESS_TERMCAP_md=$'\E[01;31m'   # begin bold
#export LESS_TERMCAP_me=$'\E[0m'       # end mode
#export LESS_TERMCAP_se=$'\E[0m'       # end standout-mode
#export LESS_TERMCAP_so=$'\E[1;33;40m' # begin standout-mode - info box
#export LESS_TERMCAP_ue=$'\E[0m'       # end underline
#export LESS_TERMCAP_us=$'\E[1;32m'    # begin underline
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[01m'          # begin underline


# Python
export PIP_REQUIRE_VIRTUALENV=1
export PIP_RESPECT_VIRTUALENV=1

export PYTHONSTARTUP=$HOME/.pythonrc.py

# Node.js
export NODE_PATH="$HOME/.node_libraries:$NODE_PATH"

# SBT
export SBT_OPTS="-Dsbt.boot.directory=$HOME/.sbt/boot/"

# RVM
#[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
