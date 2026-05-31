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

# Completions — fpath additions before compinit; regenerate cache once per day.
# Everything that should complete must be on fpath BEFORE compinit: a later
# compinit (e.g. the driving repo's) wipes any runtime `compdef`, but reloads
# fpath-based completions from the dump every time.
fpath+=("$HOME/.zfunc")
[[ -d "$HOME/.docker/completions" ]] && fpath=("$HOME/.docker/completions" $fpath)
[[ -r "$HOME/.bun/_bun" ]] && fpath=("$HOME/.bun" $fpath)  # bun ships #compdef in _bun

# fd ships no static completion; generate one into ~/.zfunc on first use. The
# file-exists check short-circuits before spawning fd, so this is a no-op on
# every later startup. To refresh after an fd upgrade: rm ~/.zfunc/_fd, new shell.
if [[ ! -e "$HOME/.zfunc/_fd" ]] && command -v fd &>/dev/null; then
    mkdir -p "$HOME/.zfunc"
    fd --gen-completions zsh > "$HOME/.zfunc/_fd" 2>/dev/null
    rm -f "$HOME/.zcompdump" "$HOME/.zcompdump.zwc"  # force compinit to pick it up
fi

autoload -Uz compinit
if [[ -n "${HOME}/.zcompdump"(#qN.mh+24) ]]; then
    compinit
else
    compinit -C
fi
# Compile the dump to bytecode so the -C fast path — and the driving repo's
# later compinit — load it faster. Recompile only when the dump is newer.
if [[ -s "$HOME/.zcompdump" && ( ! -s "$HOME/.zcompdump.zwc" || "$HOME/.zcompdump" -nt "$HOME/.zcompdump.zwc" ) ]]; then
    zcompile "$HOME/.zcompdump"
fi
