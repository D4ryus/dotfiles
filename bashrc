# file: ~/.bashrc
# author: d4ryus - https://github.com/d4ryus/
# vim:ts=2:sw=2:ai:ft=sh:

set -o vi mode

export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export PAGER=less
export EDITOR=/usr/bin/vim
export PATH=$PATH:/home/d4ryus/bin
export vrc=~/.vimrc
if [[ -z "$GOPATH" ]]; then
        export GOPATH=/home/d4ryus/gocode
fi

alias io="iostat -hmd 1"
alias waf="watch -n 1 du -sch"
alias lock="sleep 1 && xset dpms force off && slock"
alias tmux="tmux -2"
alias tm="tmux new-session -t 'd4ryus' || tmux new-session -s 'd4ryus'"
alias svnlog="svn log -v | vim -"
alias ls="ls -l --color=auto"
alias grep="grep --color=auto"
alias dmesg="dmesg --color"
alias pg="ps -efa | grep "
alias il="ip a"
alias wu="ip link set wlp3s0 up"
alias wd="ip link set wlp3s0 down"
alias we="iwconfig wlp3s0 essid "
alias ws="iwlist scan | grep 'ESSID:'"
alias ww="wpa_supplicant -i wlp3s0 -B -c "
alias wi="dhcpcd wlp3s0"
alias eu="ip link set enp0s25 up"
alias ed="ip link set enp0s25 down"
alias ei="dhcpcd enp0s25"
alias upAur="mkdir /tmp/xxx && cd /tmp/xxx && cower -ud && find -name PKGBUILD -execdir makepkg -si \; && cd $HOME && rm -rf /tmp/xxx"
alias myip="curl http://myip.dnsomatic.com && echo ''"

# git autocompletion
source ~/.git-completion-bash

# setting to display java applications correctly
if [[ $DISPLAY != "" ]]; then
        wmname LG3D
fi

# set PS1
if [[ $EUID -ne 0 ]]; then
        # user = color = green
        PS1='[\[\033[0;32m\]\h\[\033[0m\] \w\[\033[0;33m\]$(git status -b 2>/dev/null | head -n 1 | grep -oE " [^ ]+$")\[\033[0m\]] '
else
        # root = color = red
        PS1='[\[\033[0;31m\]\h\[\033[0m\] \w\[\033[0;33m\]$(git status -b 2>/dev/null | head -n 1 | grep -oE " [^ ]+$")\[\033[0m\]] '
fi

# tmux and wikidates setting
if [[ $(cat /proc/$PPID/status | head -1 | cut -f2) != "sshd" ]]; then
        if [[ "$TERM" != "screen-256color" && "$TERM" != "linux" ]]; then
                tmux new-session -t "d4ryus" || tmux new-session -s "d4ryus"
        fi
        if [ -d ~/.wikidates ]; then
                cat ~/.wikidates/$(date +%B_%d) | shuf -n 1
        fi
fi

# show 256 colors
show_colors() {
        for i in {0..255}
        do
                printf "\x1b[38;5;${i}mcolour${i}\n"
        done
}

# ping and pipe into statusbar
pong() {
        for ((i=0;i<10;i++))
        do
                ping -c 1 8.8.8.8 | sed -z s/\.\*time=// | stat_msg
                sleep 1
        done
        stat_msg 0
}

# Extract Files
extract() {
        if [ -f $1 ]; then
                case $1 in
                        *.tar.bz2)   tar xvjf $1    ;;
                        *.tar.gz)    tar xvzf $1    ;;
                        *.tbz2)      tar xvjf $1    ;;
                        *.tgz)       tar xvzf $1    ;;
                        *.tar)       tar xvf $1     ;;
                        *.tar.xz)    tar xvJf $1    ;;
                        *.Z)         uncompress $1  ;;
                        *.rar)       unrar x $1     ;;
                        *.zip|*.jar) unzip $1       ;;
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

# upload files to transer.sh
upload() {
        ARGS="--upload-file"

        if [ -f $1 ]; then
                if [[ $2 == "" ]]; then
                        curl $ARGS $1 http://transfer.sh/$1
                else
                        curl $ARGS $1 http://transfer.sh/$2
                fi
        else
                echo "\`$1' is not a valid file"
        fi
}

# sync with cub
sc() {
        ARGS="--delete --progress --recursive --times --human-readable"
        FOLDER=~/sync

        if [[ $1 == "d4" ]]; then
                rsync $ARGS d4:$FOLDER $HOME
        else
                rsync $ARGS cub:$FOLDER $HOME
        fi
}

# push to cub
push() {
        ARGS="-r"
        FOLDER=~/sync/push/

        if [[ $2 != "" ]]; then
                scp $ARGS $2 $1:$FOLDER
        else
                scp $ARGS $1 d4:$FOLDER
        fi
}
