#!/bin/bash
# -*- mode: shell-script; indent-tabs-mode: nil; sh-basic-offset: 4; -*-
# file: ~/.bashrc
# author: d4ryus - https://github.com/d4ryus/
# vim:ts=4:sw=4:ai:ft=sh:

add_path () {
    # return "$1:$2", unless $2 already contains $1. If $2 is empty, return $1
    if test -z "$2"; then
        echo "$1"
    else
        if [[ ":$2:" == *"$1"* ]]; then
            echo "$2"
        else
            echo "$1:$2"
        fi
    fi
}

export PATH="$(add_path "${HOME}/bin" "$PATH")"
export PATH="$(add_path "${HOME}/.cargo/bin" "$PATH")"
export PAGER=less
export EDITOR=vim
export HISTCONTROL=ignoreboth
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT='%F %T '
if test -z "$SSH_AUTH_SOCK"; then
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
fi
if test -z "$GOPATH"; then
    export GOPATH="${HOME}/go"
fi

alias tm="tmux attach -t"
alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias dmesg="dmesg --color"

# git autocompletion
if test -r "${HOME}/.git-completion-bash"; then
    source "${HOME}/.git-completion-bash"
fi

et() {
    local tmp
    if [[ "$1" != "-" ]]; then
        emacsclient -c -t -e "(find-file \"$1\")"
        return 0
    fi
    tmp="$(mktemp /tmp/et-stdin-XXXXXX)";
    cat > "$tmp";
    emacsclient --eval '(let ((file "'${tmp}'"))
                          (switch-to-buffer (get-buffer-create "*stdin*"))
                          (erase-buffer)
                          (insert-file-contents file)
                          (delete-file file))'
}

find-file() {
    emacsclient -n "$@"
}

# show 256 colors
show_colors() {
    for i in {0..255}; do
        printf "\x1b[38;5;%smcolour%s\n" "${i}" "${i}"
    done
}

# Extract Files
ext() {
    if ! test -e "$1"; then
        echo "'$1' is not a valid file"
        return 1
    fi
    case $1 in
        *.tar)            tar xvf "$1"    ;;
        *.tar.gz|*.tgz)   tar xvzf "$1"   ;;
        *.tar.bz2|*.tbz2) tar xvjf "$1"   ;;
        *.tar.xz)         tar xvJf "$1"   ;;
        *.Z)              uncompress "$1" ;;
        *.rar)            unrar x "$1"    ;;
        *.xz)             unxz "$1"       ;;
        *.zip|*.jar)      unzip "$1"      ;;
        *.bz2)            bunzip2 "$1"    ;;
        *.gz)             gunzip "$1"     ;;
        *.7z)             7z x "$1"       ;;
        *.exe)            cabextract "$1" ;;
        *) echo "'$1': unrecognized file compression" ;;
    esac
}

# colorized manpages, copied that from:
# https://gist.github.com/cocoalabs/2fb7dc2199b0d4bf160364b8e557eb66
man() {
    env LESS_TERMCAP_mb="$(printf "\e[1;31m")"    \
        LESS_TERMCAP_md="$(printf "\e[1;31m")"    \
        LESS_TERMCAP_me="$(printf "\e[0m")"       \
        LESS_TERMCAP_se="$(printf "\e[0m")"       \
        LESS_TERMCAP_so="$(printf "\e[1;44;33m")" \
        LESS_TERMCAP_ue="$(printf "\e[0m")"       \
        LESS_TERMCAP_us="$(printf "\e[1;32m")"    \
        man "$@"
}

__esc() {
    printf "\[\x1b%s\]" "$1"
}

__reset=$(printf $(__esc "[0m"))
__red=$(printf $(__esc "[38;5;1m"))
__green=$(printf $(__esc "[38;5;2m"))
__hostname=$(hostname)

__set_ps1() {
    local last_exit_code=$?
    # Surrouned by \[ \] so PS1 width calc does not get confused
    # \033              Escape
    # <ESC>[            CSI (Control Sequence Introducer)
    # <CSI>n<literal m> SGR (Select Graphic Rendition) where n is a
    #                   subsequence with:
    #  0                being reset, and
    # 38                set foreground color, which is followed by
    # 5;x               with x being a color code (1 red, 2 green, etc)
    # So:
    #  ,-------------,-> Escaping for PS1 width
    #  |             |
    # \[\033[38;5;1m\]
    #       | | | ||
    #       `------`---> SGR sequence <ESC>[<Graphic Rendition>m
    #         | | |
    #         | | `----> red
    #         | `------> use color code
    #         `--------> Set foreground color
    local title=$(printf $(__esc "]0;%s\a") "${__hostname} ${PWD}")
    PS1="${title}${__reset}["
    # PS1 user color
    if [[ $EUID -ne 0 ]]; then
        # user = green
        PS1+="${__green}"
    else
        # root = red
        PS1+="${__red}"
    fi
    # user and current path
    PS1+="\h${__reset} \W${__reset}"
    if ! test $last_exit_code -eq 0; then
        PS1+=" ${__red}${last_exit_code}${__reset}"
    fi
    PS1+="] "
}

if ! test "dumb" = "${TERM:-dumb}"; then
    PROMPT_COMMAND=__set_ps1
    __set_ps1
fi

if test -r "${HOME}/.bashrc.local"; then
    source "${HOME}/.bashrc.local"
fi
