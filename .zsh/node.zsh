# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
[[ -s "$HOME/.bun/_bun" ]] && source "$HOME/.bun/_bun"
alias br='bun run'

# Node version manager — prefer fnm (fast), fall back to lazy-loaded NVM
# Lazy loading NVM saves ~0.5–1s on shell startup; it activates on first use.
if command -v fnm &>/dev/null; then
    eval "$(fnm env --use-on-cd --shell zsh)"
else
    _nvm_load() {
        unset -f nvm node npm npx yarn
        [[ -s "/opt/homebrew/opt/nvm/nvm.sh" ]] && source "/opt/homebrew/opt/nvm/nvm.sh"
        [[ -s "$HOME/.nvm/nvm.sh" ]]             && source "$HOME/.nvm/nvm.sh"
    }
    nvm()  { _nvm_load; nvm  "$@" }
    node() { _nvm_load; node "$@" }
    npm()  { _nvm_load; npm  "$@" }
    npx()  { _nvm_load; npx  "$@" }
    yarn() { _nvm_load; yarn "$@" }
fi
