# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="minimal"

export DOCKER_HOST=tcp://localhost:4243
export TERM="xterm-256color"
export EDITOR=vim
export NODE_PATH=$NODE_PATH:/usr/local/lib/node_modules

# cache pip-installed packages to avoid re-downloading
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache

#Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"
#
DISABLE_CORRECTION="true"

REPORTTIME=5

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git vi-mode brew osx history history-substring-search)

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

source $ZSH/oh-my-zsh.sh

alias bibtex2html="export TMPDIR=. && /usr/local/bin/bibtex2html"

# Customize to your needs...
export PATH="/sbin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/local/share:$HOME/bin:/usr/bin:/opt/bin:$PATH"

# Put secret configuration settings in ~/.secrets
if [[ -a ~/.secrets ]] then
  source ~/.secrets
fi

PATH=$PATH:/usr/local/texlive/2014/bin/universal-darwin # Add pdflatex
PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
