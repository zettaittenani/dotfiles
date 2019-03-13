autoload -U compinit
compinit

autoload -Uz colors
colors

setopt COMPLETE_IN_WORD

PROMPT='${vcs_info_msg_0_}$ '

export LANG=ja_JP.UTF-8
export HISTFILE=${HOME}/.zsh_history
export HISTSIZE=1000
export SAVEHIST=100000
export ZLS_COLORS=$LS_COLORS
setopt hist_ignore_dups
setopt EXTENDED_HISTORY

source ~/.zsh_plugins/zaw/zaw.zsh
zstyle ':filter-select:highlight' selected fg=black,bg=white,standout
zstyle ':filter-select' case-insensitive yes

bindkey '^@' zaw-cdr
bindkey '^R' zaw-history
# bindkey '^F' zaw-git-files
# bindkey '^Q' zaw-git-branches
# bindkey '^P' zaw-process
# bindkey '^X^A' zaw-tmux

autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 500
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# Colors settings
setopt prompt_subst
setopt auto_resume
setopt auto_list
setopt auto_cd
setopt auto_pushd

# Git alias
alias gg="git grep"
alias ga="git add"
alias gc="git commit"
alias gp="git push"
alias gd="git diff"
alias gst="git status"
alias gl="git log"
alias gb="git branch"

# Emacs settings
alias emacsclient="/usr/local/bin/emacsclient"
alias ess="emacs --daemon"
alias esk="emacsclient -e '(kill-emacs)'"
alias e="emacsclient -nw -c -a emacs -nw"

# Other aliases
alias ls='ls -la -G'
alias awk='gawk'
alias dc='docker-compose'
