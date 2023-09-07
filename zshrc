# Compatibility for emacs tramp
[[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return

export EDITOR=vim

export NODE_HOME=~/Sync/etc/node-v20.5.1-linux-x64
export JAVA_HOME=~/Sync/etc/graalvm-jdk-20.0.2+9.1

# CUDA Stuff
export CUDA_HOME=/usr/local/cuda
export LD_LIBRARY_PATH=$CUDA_HOME/lib64:$LD_LIBRARY_PATH

# CUDA Compiler (nvcc)
export CUDACXX=$CUDA_HOME/bin/nvcc

export PATH=$PATH:$NODE_HOME/bin/:~/bin/:~/.local/bin/
# export LLVM_TOOLCHAIN=$(lli --print-toolchain-path)

source ~/dotfiles/antigen.zsh

DISABLE_LS_COLORS="true"

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle gitfast
antigen bundle fzf
antigen bundle history-substring-search
antigen bundle jeffreytse/zsh-vi-mode

# Load the theme.
antigen theme minimal

# Tell Antigen that you're done.
antigen apply

# Some of the most useful features in emacs-libvterm require shell-side
# configurations. The main goal of these additional functions is to enable the
# shell to send information to `vterm` via properly escaped sequences. A
# function that helps in this task, `vterm_printf`, is defined below.

function vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Completely clear the buffer. With this, everything that is not on screen
# is erased.
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# With vterm_cmd you can execute Emacs commands directly from the shell.
# For example, vterm_cmd message "HI" will print "HI".
# To enable new commands, you have to customize Emacs's variable
# vterm-eval-cmds.
vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

# This is to change the title of the buffer based on information provided by the
# shell. See, http://tldp.org/HOWTO/Xterm-Title-4.html, for the meaning of the
# various symbols.
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

# Sync directory and host in the shell with Emacs's current directory.
# You may need to manually specify the hostname instead of $(hostname) in case
# $(hostname) does not return the correct string to connect to the server.
#
# The escape sequence "51;A" has also the role of identifying the end of the
# prompt
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

eval "$(direnv hook zsh)"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

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

