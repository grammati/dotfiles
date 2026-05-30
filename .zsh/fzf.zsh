# fzf — shell integration (key bindings + completions)
# Try locations in preference order: ~/.fzf.zsh, homebrew, apt, cargo-installed
if [[ -f ~/.fzf.zsh ]]; then
    source ~/.fzf.zsh
elif [[ -d /opt/homebrew/opt/fzf/shell ]]; then
    source /opt/homebrew/opt/fzf/shell/key-bindings.zsh
    source /opt/homebrew/opt/fzf/shell/completion.zsh 2>/dev/null
elif [[ -f /usr/share/doc/fzf/examples/key-bindings.zsh ]]; then
    source /usr/share/doc/fzf/examples/key-bindings.zsh
    source /usr/share/doc/fzf/examples/completion.zsh 2>/dev/null
fi

if command -v fzf &>/dev/null; then
    if command -v fd &>/dev/null; then
        export FZF_DEFAULT_COMMAND="fd --color=always"
        export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
        export FZF_ALT_C_COMMAND="fd --type dir"
    fi
    export FZF_DEFAULT_OPTS="--ansi --style full"

    # ^R history: hide the leading history-index column (fzf still uses it
    # internally to look up the command; --with-nth only affects display).
    export FZF_CTRL_R_OPTS="--with-nth=2.."

    _fzf_comprun() {
        local command=$1; shift
        case "$command" in
            cd)           fd --type dir | fzf --preview "tree -L 2 {}" "$@" ;;
            export|unset) fzf --preview "eval 'echo \$'{}" "$@" ;;
            ssh)          fzf --preview 'dig {}' "$@" ;;
            *)            fzf "$@" ;;
        esac
    }

    # ^B — git branch picker (replaces zaw-git-recent-branches)
    _fzf_git_branch() {
        local branch
        branch=$(git branch -a --sort=-committerdate 2>/dev/null |
            grep -v HEAD | sed 's|remotes/origin/||' | sort -u |
            fzf --height=40% --reverse --preview='git log --oneline -10 {}') || return
        LBUFFER+="${branch// /}"
        zle reset-prompt
    }
    zle -N _fzf_git_branch
    bindkey '^B' _fzf_git_branch
fi
