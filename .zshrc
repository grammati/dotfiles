# Source Zgen and create init file if necessary
# Add custom Zsh plugins to .zgen-setup.zsh
source ~/.zgen-setup.zsh

# Add customizations below

################################################################################
# Chris's stuff

# for npm and all that crap
# ulimit -n 8192

zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# Do not share history between tmux panes. Yay!
setopt nosharehistory

export EMACS_HOME=/Applications/Emacs.app/Contents/MacOS
alias emacs="$EMACS_HOME/Emacs -nw --insecure"
alias emacsw="$EMACS_HOME/Emacs --insecure"
export EDITOR='emacsclient -t --alternate-editor=runemacs'
alias e='emacsclient -t --alternate-editor=runemacs'

export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

alias lr='ls -Altr'

alias gp='git pull'
alias gl='git lg'
alias gs='git status'
alias gr='git remote'
alias grv='git remote -rv'
alias lga='lg --all'

alias t2='tree -L 2'
alias t3='tree -L 3'

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
gi() {
    cmd=`echo $1 | cut -c 2-`
    shift
    git $cmd $*
}

portfwd() {
    args=''
    for arg in $*; do
        args="$args -L ${arg}:localhost:${arg}";
    done
    cmd="ssh -N ${args} chperki.aka.corp.amazon.com"
    echo $cmd
    eval $cmd
}

fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit && compinit -i

# AWS Completion
source /usr/local/bin/aws_zsh_completer.sh

# RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
