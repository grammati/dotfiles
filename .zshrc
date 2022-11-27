##################################
## oh-my-zsh setup
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="af-magic"
DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(asdf aws vi-mode zsh-better-npm-completion)
source $ZSH/oh-my-zsh.sh

###################################
## vi-mode

# Bar cursor in insert mode, block in normal
export VI_MODE_SET_CURSOR=true

###################################
## Shell / Misc
export PATH=$HOME/bin:$PATH

export EDITOR=nvim
export VISUAL=nvim

alias lr='ls -Altr'
alias t2='tree -L 2'
alias t3='tree -L 3'

###################################
## zaw
source $HOME/projects/zaw/zaw.zsh

zstyle ':filter-select' max-lines 10
zstyle ':filter-select' max-lines -10
zstyle ':filter-select' hist-find-no-dups yes
zstyle ':filter-select' extended-search yes
zstyle ':filter-select' case-insensitive yes
zstyle ':filter-select' rotate-list yes
zstyle ':filter-select:highlight' selected fg=cyan,underline
zstyle ':filter-select:highlight' matched fg=yellow
zstyle ':filter-select:highlight' title fg=yellow,underline
zstyle ':filter-select:highlight' marked bg=blue

bindkey '^R^R' zaw-history

bindkey -M filterselect '^J' down-line-or-history
bindkey -M filterselect '^K' up-line-or-history
bindkey -M filterselect '^[^M' accept-search # Alt-Enter to accept but not execute

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
