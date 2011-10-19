# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="nicoulaj"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# export DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh
source ~/dotfiles/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh

# Customize to your needs...
#
export EDITOR=vim
export PAGER=most
export BROWSER=chromium

# Aliases
alias grep='grep --color=auto'
alias df='df --human-readable'
alias du='du --human-readable'
alias duh='du --all --max-depth=1|sort -n'
alias d='dirs -v'
alias less='less --ignore-case'

# git aliases (thx to Peteris Krumins)
alias ga='git add'
alias gp='git push'
alias gl='git log'
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gm='git commit -m'
alias gma='git commit -am'
alias gb='git branch'
alias gc='git checkout'
alias gra='git remote add'
alias grr='git remote rm'
alias gpu='git pull'
alias gcl='git clone'

s() { echo "$*" | tr " " "+" | xargs -I %s xdg-open "http://google.com/search?q=%s" }

# Keybindings
bindkey "OH"      beginning-of-line   # Home
bindkey "OF"      end-of-line         # End
bindkey "[1;5D"   backward-word       # Left arrow
bindkey "[1;5C"   forward-word        # Right arrow
bindkey "[3~"     delete-char         # Delete
