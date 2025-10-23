# Compatibility for emacs tramp
[[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return

##########################################
# vi mode
# https://rm-rf.ca/posts/2020/zsh-vi-mode/
##########################################

LANG=en_US.UTF-8
LC_ALL=
LC_TIME=C.UTF-8

bindkey -v

# `v` is already mapped to visual mode, so we need to use a different key to
# open Vim
bindkey -M vicmd "^V" edit-command-line

# Make Vi mode transitions faster (KEYTIMEOUT is in hundredths of a second)
export KEYTIMEOUT=1

# Bind both CSI (^[[A/^[[B) and SS3 (^[OA/^[OB) variants, in all keymaps.
for keymap in emacs viins vicmd; do
  # Up
  bindkey -M $keymap "${terminfo[kcuu1]:-\e[A]}" history-substring-search-up
  bindkey -M $keymap "\eOA"                      history-substring-search-up
  # Down
  bindkey -M $keymap "${terminfo[kcud1]:-\e[B]}" history-substring-search-down
  bindkey -M $keymap "\eOB"                      history-substring-search-down
done

bindkey -M vicmd "k" history-substring-search-up
bindkey -M vicmd "j" history-substring-search-down

bindkey "^F" history-incremental-search-forward
bindkey "^R" history-incremental-search-backward
bindkey -M viins '^R' history-incremental-pattern-search-backward
bindkey -M viins '^F' history-incremental-pattern-search-forward
bindkey "^P" history-search-backward
bindkey "^N" history-search-forward

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
# sorry
alias nano='vim'
alias reallynano='/usr/bin/nano'

export NODE_HOME=~/Sync/etc/node-v20.5.1-linux-x64
export JAVA_HOME=~/Sync/etc/graalvm-jdk-21+35.1

# export PATH=$PATH:$NODE_HOME/bin/:~/bin/:~/.local/bin/:~/Sync/bin/
# export LLVM_TOOLCHAIN=$(lli --print-toolchain-path)

# CUDA
export CUDA_HOME=/usr/local/cuda-13.0
export PATH=$CUDA_HOME/bin:$PATH
export LD_LIBRARY_PATH=$CUDA_HOME/lib64:${LD_LIBRARY_PATH:-}

eval "$(direnv hook zsh)"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Load the theme.
source ~/dotfiles/minimal.zsh-theme

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/home/joelkuiper/Sync/etc/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/home/joelkuiper/Sync/etc/miniconda3/etc/profile.d/conda.sh" ]; then
#         . "/home/joelkuiper/Sync/etc/miniconda3/etc/profile.d/conda.sh"
#     else
#         export PATH="/home/joelkuiper/Sync/etc/miniconda3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# <<< conda initialize <<<

# Put secret configuration settings in ~/.secrets
if [[ -a ~/.secrets ]] then
  source ~/.secrets
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
# export SDKMAN_DIR="$HOME/.sdkman"
# [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
