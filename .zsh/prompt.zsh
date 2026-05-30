if command -v starship &>/dev/null; then
    eval "$(starship init zsh)"
else
    autoload -Uz vcs_info
    precmd_vcs_info() { vcs_info }
    precmd_functions+=(precmd_vcs_info)
    zstyle ':vcs_info:git:*' formats ' (%b)'
    setopt PROMPT_SUBST
    PROMPT='%F{cyan}%~%f%F{yellow}${vcs_info_msg_0_}%f
%# '
fi
