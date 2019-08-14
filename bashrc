# -*- mode: shell-script; indent-tabs-mode: nil; sh-basic-offset: 4; -*-
# file: ~/.bashrc
# author: d4ryus - https://github.com/d4ryus/
# vim:ts=4:sw=4:ai:ft=sh:

add_path () {
    # add $1 to $2 with a : in between, unless $2 already contains
    # $1. If $2 is empty set it to $1
    if [ -z "$2" ]; then
        echo "$1"
    else
        if [[ ":$2:" == *"$1"*  ]]; then
            echo "$2"
        else
            echo "$2:$1"
        fi
    fi
}

export PATH=$(add_path "${HOME}/bin" "$PATH")
export LIBRARY_PATH=$(add_path "${HOME}/lib" "$LIBRARY_PATH")
export LD_LIBRARY_PATH=$(add_path "${HOME}/lib" "$LD_LIBRARY_PATH")
export C_INCLUDE_PATH=$(add_path "${HOME}/include" "$C_INCLUDE_PATH")
export PAGER=less
export EDITOR=vim
export SDL_AUDIODRIVER=pulse
export HISTCONTROL=ignoredups
if [[ -z "$GOPATH" ]]; then
    export GOPATH="${HOME}/go"
fi

alias tm="tmux attach -t"
alias ts="tmux ls"
alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias dmesg="dmesg --color"
alias myip="curl http://myip.dnsomatic.com && echo ''"
alias ec="emacsclient -a \"\" -c -n"

# git autocompletion
if [ -r "${HOME}/.git-completion-bash" ]; then
    source "${HOME}/.git-completion-bash"
fi

if [ -r "${HOME}/.bashrc.local" ]; then
    source "${HOME}/.bashrc.local"
fi

et() {
    local TMP
    if [[ "$1" != "-" ]]; then
        emacsclient -a "" -c -t -e "(find-file \"$1\")"
        return 0
    fi
    TMP="$(mktemp /tmp/et-stdin-XXXXXX)";
    cat > "$TMP";
    emacsclient --eval '(let ((file "'${TMP}'"))
                          (switch-to-buffer (get-buffer-create "*stdin*"))
                          (erase-buffer)
                          (insert-file-contents file)
                          (delete-file file))'
}

doit() {
    emacsclient -a "" -n "$@"
}

# show 256 colors
show_colors() {
    for i in {0..255}; do
        printf "\x1b[38;5;${i}mcolour${i}\n"
    done
}

# Extract Files
ext() {
    if [ -f "$1" ]; then
        case $1 in
            *.tar.bz2)   tar xvjf "$1"    ;;
            *.tar.gz)    tar xvzf "$1"    ;;
            *.tbz2)      tar xvjf "$1"    ;;
            *.tgz)       tar xvzf "$1"    ;;
            *.tar)       tar xvf "$1"     ;;
            *.tar.xz)    tar xvJf "$1"    ;;
            *.Z)         uncompress "$1"  ;;
            *.rar)       unrar x "$1"     ;;
            *.zip|*.jar) unzip "$1"       ;;
            *.gz)        gunzip "$1"      ;;
            *.bz2)       bunzip2 "$1"     ;;
            *.7z)        7z x "$1"        ;;
            *.xz)        unxz "$1"        ;;
            *.exe)       cabextract "$1"  ;;
            *)           echo "\`$1': unrecognized file compression" ;;
        esac
    else
        echo "\`$1' is not a valid file"
    fi
}

# colorized manpages, copied that from:
# https://gist.github.com/cocoalabs/2fb7dc2199b0d4bf160364b8e557eb66
man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}

set_ps1() {
    local reset=$(tput sgr0)
    local red=$(tput setaf 1)
    local green=$(tput setaf 2)

    PS1="\[$reset\]["
    # PS1 user color
    if [[ $EUID -ne 0 ]]; then
        # user = green
        PS1+="\[$green\]"
    else
        # root = red
        PS1+="\[$red\]"
    fi
    # user and current path
    PS1+="\h\[$reset\] \W\[$reset\]] "
}

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > "${HOME}/.ssh-agent-environment"
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(cat ${HOME}/.ssh-agent-environment)"
fi

set_ps1
