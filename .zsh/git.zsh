alias gs='git status'
alias lg='git lg'
alias gl='git pull'
alias gd='git diff'
alias gds='git diff --staged'
alias gco='git checkout'
alias gw='git switch'
alias gwt='git worktree'
alias gad='git add .'
alias gcm='git commit -m'
alias gca='git commit --amend'
alias gcan='git commit --amend --no-edit'
alias gfo='git fetch origin'
alias grv='git remote -v'
alias gpor='git pull origin master --rebase'
alias grc='git rebase --continue'
alias gsu='git rev-parse --abbrev-ref @{u}'

# Fetch a remote branch and check it out locally
gcor() { git fetch origin "$1" && git checkout -b "$1" FETCH_HEAD }

# Get the actual merge commit SHA for a merged PR
prsha() {
    local pr="${1:?Usage: prsha <PR-number>}"
    local sha
    sha=$(gh pr view "$pr" --json mergeCommit --jq '.mergeCommit.oid') || return 1
    [[ -z "$sha" ]] && { echo "Not merged"; return 1 }
    echo "$sha"
}

# Show git log for tags matching a prefix
logtag() {
    git log --abbrev-commit --decorate \
        --format='%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' \
        --no-walk --tags="${1}*"
}
