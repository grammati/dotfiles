export DOTFILES="$HOME/personal/dotfiles"

for f in "$DOTFILES/.zsh"/*.zsh; do
    [[ -r "$f" ]] && source "$f"
done
unset f
