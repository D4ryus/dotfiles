# file: ~/.zshrc
# author: d4ryus - https://github.com/d4ryus/
# vim:ts=2:sw=2:ai:ft=sh:
#
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000000
setopt appendhistory autocd extendedglob notify
# vim keys
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/d4ryus/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

set -o vi mode

source ~/.git-prompt.sh

export EDITOR=/usr/bin/vim
export PATH=$PATH:/home/d4ryus/bin
export vrc=~/.vimrc

alias lock="sleep 1 && xset dpms force off && slock"
alias tmux="tmux -2"
alias svnlog="svn log -v | vim -"
alias ls="ls -l --color=auto"
alias grep="grep --color=auto"
alias dmesg="dmesg --color"
alias pg="ps -efa | grep "
alias il="ip a"
alias wu="ip link set wlp3s0 up"
alias wd="ip link set wlp3s0 down"
alias we="iwconfig wlp3s0 essid "
alias ws="iwlist scan | grep 'ESSID:' | awk {'print $1'}"
alias ww="wpa_supplicant -i wlp3s0 -B -c "
alias wi="dhcpcd wlp3s0"
alias eu="ip link set enp0s25 up"
alias ed="ip link set enp0s25 down"
alias ei="dhcpcd enp0s25"

# ping and pipe into statusbar
pong() {
  for ((i=0;i<10;i++))
  do
    ping -c 1 8.8.8.8 | sed -z s/\.\*time=// | stat_msg
    sleep 1
  done
  stat_msg 0
}

#Estract Files
extract() {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2) tar xvjf $1   ;;
      *.tar.gz)  tar xvzf $1   ;;
      *.tbz2)    tar xvjf $1   ;;
      *.tgz)     tar xvzf $1   ;;
      *.tar)     tar xvf $1    ;;
      *.tar.xz)  tar xvJf $1   ;;
      *.Z)       uncompress $1 ;;
      *.rar)     unrar x $1    ;;
      *.zip)     unzip $1      ;;
      *.gz)      gunzip $1     ;;
      *.bz2)     bunzip2 $1    ;;
      *.7z)      7z x $1       ;;
      *.xz)      unxz $1       ;;
      *.exe)     cabextract $1 ;;
      *)         echo "\`$1': unrecognized file compression" ;;
    esac
  else
    echo "\`$1' is not a valid file"
  fi
}

autoload -U promptinit && promptinit
autoload -U colors && colors
setopt PROMPT_SUBST;
if [[ $EUID -ne 0 ]];
  then
    # user = color = green
    PROMPT='[%{$fg[green]%}%n%{$reset_color%} %~%{$fg[yellow]%}$(__git_ps1 " %s")%{$reset_color%}] '
  else
    # root = color = red
    PROMPT='[%{$fg[red]%}%n%{$reset_color%} %~%{$fg[yellow]%}$(__git_ps1 " %s")%{$reset_color%}] '
fi
