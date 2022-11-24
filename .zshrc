##################################
## oh-my-zsh setup
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="chris"
DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(aws git vi-mode zsh-better-npm-completion)
source $ZSH/oh-my-zsh.sh

###################################
## Shell / Misc
export PATH=$HOME/bin:$PATH

alias lr='ls -Altr'
alias t2='tree -L 2'
alias t3='tree -L 3'

###################################
# AWS
export AWS_PAGER=""

###################################
# Emacs
alias em='emacs'
alias e='emacsclient'

###################################
# Node
alias nr='npm run'
alias ni='npm install'

###################################
## Functions

gi() {
    cmd=`echo $1 | cut -c 2-`
    shift
    git $cmd $*
}

portfwd() {
    cmd="ssh -N -L $1:localhost:$1 $2"
    echo $cmd
    eval $cmd
}

###################################
# Terraform
autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /opt/homebrew/bin/terraform terraform

###################################
# Python
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/chris/opt/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/chris/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/chris/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/chris/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

###################################
## NVM
export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

