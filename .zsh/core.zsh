# PATH
export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.cargo/bin:$PATH"

# Editor — don't override if already set (e.g. by Cursor/VSCode)
[[ -z "$EDITOR" ]] && export EDITOR="nvim"
[[ -z "$VISUAL" ]] && export VISUAL="nvim"

# AWS
export AWS_PAGER=""

# History
HISTFILE="$HOME/.zsh_history"
HISTSIZE=100000
SAVEHIST=100000
setopt HIST_IGNORE_DUPS HIST_IGNORE_SPACE HIST_FIND_NO_DUPS SHARE_HISTORY EXTENDED_HISTORY

# Directory
setopt AUTO_CD NO_BEEP

# Completions — fpath additions before compinit; regenerate cache once per day
fpath+=("$HOME/.zfunc")
[[ -d "$HOME/.docker/completions" ]] && fpath=("$HOME/.docker/completions" $fpath)
autoload -Uz compinit
if [[ -n "${HOME}/.zcompdump"(#qN.mh+24) ]]; then
    compinit
else
    compinit -C
fi
