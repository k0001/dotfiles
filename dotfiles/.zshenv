export DEBFULLNAME="Renzo Carbonara"
export DEBEMAIL="gnuk0001@gmail.com"

export EDITOR=$(which vim)

# Colors
export TERM=xterm-256color

export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;31'

export LESS="FSRX"
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline


# Python
export PIP_REQUIRE_VIRTUALENV=1
export PIP_RESPECT_VIRTUALENV=1

export PYTHONSTARTUP=$HOME/.pythonrc.py

# Node.js
export NODE_PATH="$HOME/.node_libraries:$NODE_PATH"

# RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

