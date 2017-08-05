# Source Zgen and create init file if necessary
# Add custom Zsh plugins to .zgen-setup.zsh
source ~/.zgen-setup.zsh

# Add customizations below

################################################################################
# Chris's stuff

# for npm and all that crap
ulimit -n 8192

alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
export EDITOR='emacsclient -t --alternate-editor=runemacs'
alias e='emacsclient -t --alternate-editor=runemacs'

alias gp='git pull'
alias gl='git lg'

alias flushdns='sudo dscacheutil -flushcache;sudo killall -HUP mDNSResponder;'

# brew install coreutils
alias truncate=gtruncate
alias trunc='gtruncate --size 0'

# Sqlplus
#export SQLPLUS_HOME=$HOME/bin/instantclient_11_2
#export PATH=$PATH:$SQLPLUS_HOME
#export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:$SQLPLUS_HOME

# Anaconda
export PATH="/Users/perch13/anaconda/bin:$PATH"

# Go
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# $HOME/bin
export PATH=$HOME/bin:$PATH

# AWS completion
source /Users/perch13/anaconda/bin/aws_zsh_completer.sh

# RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
