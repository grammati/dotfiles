# vi-mode
bindkey -v
export KEYTIMEOUT=1

# fzf.zsh binds ^R in the emacs keymap before this file runs.
# `bindkey -v` switches the active keymap to viins, losing that binding.
# Rebind it explicitly so typed-prefix ^R history search still works.
if (( ${+functions[fzf-history-widget]} )); then
    bindkey '^R' fzf-history-widget
fi

# Cursor shape: beam in insert, block in normal
zle-keymap-select() {
    [[ $KEYMAP == vicmd ]] && printf '\e[1 q' || printf '\e[5 q'
}
zle-line-init() { printf '\e[5 q' }
zle -N zle-keymap-select
zle -N zle-line-init

# Restore useful insert-mode bindings that vi-mode removes
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
bindkey '^W' backward-kill-word
bindkey '^U' backward-kill-line
bindkey '^P' up-line-or-history    # previous history (like up arrow)
bindkey '^N' down-line-or-history  # next history (like down arrow)

# Edit current command line in an editor.
# Use Cursor only when running inside Cursor's integrated terminal
# (CURSOR_TRACE_ID is set there); elsewhere fall back to $VISUAL/$EDITOR.
autoload -U edit-command-line
zle -N edit-command-line
editcli() {
    if [[ -n "$CURSOR_TRACE_ID" ]]; then
        VISUAL='cursor --wait' zle edit-command-line
    else
        zle edit-command-line
    fi
}
zle -N editcli
bindkey '\C-x\C-e' editcli
bindkey -M vicmd 'vv' editcli
