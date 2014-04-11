#
# /etc/bash.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion

export EDITOR=/usr/bin/vim
export PATH=$PATH:/home/d4ryus/bin
export vrc=~/.vim/.vimrc
export brc=/etc/bash.bashrc

alias ls="ls --color=auto"
alias dir="dir --color=auto"
alias grep="grep --color=auto"
alias dmesg="dmesg --color"

#Estract Files
extract() {
  if [ -f $1 ] ; then
      case $1 in
          *.tar.bz2)   tar xvjf $1    ;;
          *.tar.gz)    tar xvzf $1    ;;
          *.tbz2)      tar xvjf $1    ;;
          *.tgz)       tar xvzf $1    ;;
          *.tar)       tar xvf $1     ;;
          *.tar.xz)    tar xvJf $1    ;;
          *.Z)         uncompress $1  ;;
          *.rar)       unrar x $1     ;;
          *.zip)       unzip $1       ;;
          *.gz)        gunzip $1      ;;
          *.bz2)       bunzip2 $1     ;;
          *.7z)        7z x $1        ;;
          *.xz)        unxz $1        ;;
          *.exe)       cabextract $1  ;;
          *)           echo "\`$1': unrecognized file compression" ;;
      esac
  else
      echo "\`$1' is not a valid file"
  fi
}

archey3

PS1="┌─[\e[0;32m\u\e[0m]-[\e[0;33m@\H\e[0m]-[\t]-[\e[0;34m\w\e[0m]\n└──[ "

if [ -f ~/.bashrc ]
    then
        source ~/.bashrc
fi
