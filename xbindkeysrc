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

if [[ $EUID -ne 0 ]]; 
    then
        # user = color = green
        PS1='\[[\033[0;32m\]\u\[\033[0m\] \w] '
    else
        # root = color = red
        PS1='\[[\033[0;31m\]\u\[\033[0m\] \w] '
        # update vim
        rsync -rth --delete --progress /home/d4ryus/.vim/ ~/.vim/
fi
