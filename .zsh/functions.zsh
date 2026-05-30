# Colorized ls — GNU coreutils uses --color, BSD/macOS ls uses -G.
# Aliasing `ls` means l/ll/la/lr inherit color via alias expansion.
if ls --color=auto >/dev/null 2>&1; then
    alias ls='ls --color=auto'
else
    alias ls='ls -G'
fi
alias l='ls -lah'
alias la='ls -la'
alias ll='ls -l'
alias lr='ls -Altrh'

# Go up N directories
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

alias t2='tree -L 2'
alias t3='tree -L 3'
alias vim='nvim'
alias nr='npm run'
alias ni='npm install'
alias pn='pnpm'
alias awswho='aws sts get-caller-identity'
alias lga='lg --all'
alias uuidgen='uuidgen | tr "[:upper:]" "[:lower:]" | tr -d "\n"'

# fasd — fast directory/file access (z, f, d, etc.)
# On Mac: ~/bin/fasd (script). On Linux: /usr/bin/fasd (binary).
if [[ -f "$HOME/bin/fasd" ]]; then
    source "$HOME/bin/fasd"
    eval "$(fasd --init auto)"
    unalias s 2>/dev/null
elif command -v fasd &>/dev/null; then
    eval "$(fasd --init auto)"
    unalias s 2>/dev/null
fi

# yazi: close and cd into the directory yazi was in
y() {
    local tmp cwd
    tmp="$(mktemp -t yazi-cwd.XXXXXX)"
    yazi "$@" --cwd-file="$tmp"
    if cwd="$(command cat -- "$tmp")" && [[ -n "$cwd" && "$cwd" != "$PWD" ]]; then
        builtin cd -- "$cwd"
    fi
    rm -f -- "$tmp"
}

# Open file/dir in $EDITOR
edit() { ${EDITOR:-nvim} "$@" }
alias e='edit'

# SSH port forward: portfwd <port> [host=vdi]
portfwd() {
    local cmd="ssh -N -L ${1}:localhost:$1 ${2:-vdi}"
    echo "$cmd"
    eval "$cmd"
}

# SSH reverse port forward: revportfwd <port> [host=vdi]
revportfwd() {
    local cmd="ssh -N -R ${1}:localhost:$1 ${2:-vdi}"
    echo "$cmd"
    eval "$cmd"
}

# Kill process(es) listening on a port: killport [-a] <port>
killport() {
    local kill_all=false
    [[ "$1" == "-a" ]] && { kill_all=true; shift }
    local port="$1"
    local pids
    pids=$(lsof -i ":$port" -t) || { echo "No process on port $port"; return }
    local count
    count=$(echo "$pids" | wc -w)
    if [[ $count -eq 1 ]]; then
        kill -9 "$pids" && echo "Killed $pids on port $port"
    elif $kill_all; then
        echo "$pids" | xargs -I{} sh -c 'echo Killing: $(ps -fp {} | tail -1); kill -9 {}'
    else
        echo "$count processes on port $port:"; echo "$pids" | xargs ps -fp
    fi
}

# Show git status summary for every child directory that is a git repo
gsall() {
    local dir
    for dir in */; do
        [[ -d "$dir" ]] || continue
        dir="${dir%/}"
        [[ $(git -C "$dir" rev-parse --is-inside-work-tree 2>/dev/null) == true ]] || continue
        echo "=== $dir ==="
        (cd "$dir" && {
            local branch
            branch=$(git branch --show-current 2>/dev/null)
            [[ -z "$branch" ]] && branch="(detached: $(git rev-parse --short HEAD))"
            echo "Branch: $branch"
            echo "Last commit: $(git log -1 --format=%cd --date=short 2>/dev/null)"
            if [[ -z $(git status --porcelain 2>/dev/null) ]]; then
                echo "Clean: yes"
            else
                echo "Clean: no"; git status -s
            fi
        })
        echo ""
    done
}

# Project navigation — all relative to the git root
ppr()  { git rev-parse --show-toplevel }
pcdr() { cd "$(ppr)" }
pfd() {
    local gitroot
    gitroot=$(ppr) || return 1
    local selected
    selected=$(fd --type dir --base-directory "$gitroot" "" |
        fzf --scheme=path --preview="tree -L 2 {}" --query="$*") || return
    echo "$gitroot/$selected"
}
pcd() {
    local selected
    selected=$(pfd "$@") && cd "$selected"
}

# Pretty-print JSON file in place
jqpretty() {
    local file="$1" tmp="${1}.tmp"
    cp "$file" "$tmp" && jq '.' "$tmp" > "$file" && rm "$tmp"
}
