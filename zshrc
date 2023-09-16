# Compatibility for emacs tramp
[[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return

##########################################
# vi mode
# https://rm-rf.ca/posts/2020/zsh-vi-mode/
##########################################

bindkey -v

# `v` is already mapped to visual mode, so we need to use a different key to
# open Vim
bindkey -M vicmd "^V" edit-command-line

# Make Vi mode transitions faster (KEYTIMEOUT is in hundredths of a second)
export KEYTIMEOUT=1

# incremental search in insert mode
bindkey "^F" history-incremental-search-forward
bindkey "^R" history-incremental-search-backward

# beginning search with arrow keys and j/k
bindkey "^[OA" up-line-or-beginning-search
bindkey "^[OB" down-line-or-beginning-search
bindkey -M vicmd "k" up-line-or-beginning-search
bindkey -M vicmd "j" down-line-or-beginning-search

# beginning search in insert mode, redundant with the up/down arrows above
# but a little easier to press.
bindkey "^P" history-search-backward
bindkey "^N" history-search-forward

# incremental search in vi command mode
bindkey -M vicmd '?' history-incremental-search-backward
bindkey -M vicmd '/' history-incremental-search-forward
# navigate matches in incremental search
bindkey -M viins '^R' history-incremental-pattern-search-backward
bindkey -M viins '^F' history-incremental-pattern-search-forward

source ~/dotfiles/antigen.zsh

DISABLE_LS_COLORS="true"

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle gitfast
antigen bundle fzf
antigen bundle vi-mode
antigen bundle history-substring-search

# Tell Antigen that you're done.
antigen apply

export EDITOR=vim

export NODE_HOME=~/Sync/etc/node-v20.5.1-linux-x64
export JAVA_HOME=~/Sync/etc/graalvm-jdk-20.0.2+9.1

# CUDA Stuff
export CUDA_HOME=/usr/local/cuda
export LD_LIBRARY_PATH=$CUDA_HOME/lib64:$LD_LIBRARY_PATH

# CUDA Compiler (nvcc)
export CUDACXX=$CUDA_HOME/bin/nvcc

export PATH=$PATH:$NODE_HOME/bin/:~/bin/:~/.local/bin/:~/Sync/bin/
# export LLVM_TOOLCHAIN=$(lli --print-toolchain-path)

eval "$(direnv hook zsh)"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Load the theme.
source ~/dotfiles/minimal.zsh-theme

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/joelkuiper/Sync/etc/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/joelkuiper/Sync/etc/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/joelkuiper/Sync/etc/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/joelkuiper/Sync/etc/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# Put secret configuration settings in ~/.secrets
if [[ -a ~/.secrets ]] then
  source ~/.secrets
fi
