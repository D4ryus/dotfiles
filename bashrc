# file: ~/.bashrc
# author: d4ryus - https://github.com/d4ryus/
# vim:ts=8:sw=8:ai:ft=sh:

export PATH=$PATH:$HOME/bin/
export LIBRARY_PATH=$LIBRARY_PATH:$HOME/lib/
export LD_LIBRARY_PATH=$LIBRARY_PATH:$HOME/lib/
export C_INCLUDE_PATH=$C_INCLUDE_PATH:$HOME/include/
export PROMPT_DIRTRIM=3
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export PAGER=less
export EDITOR=et-wrapper
export vrc=~/.vimrc
export SDL_AUDIODRIVER=alsa
if [[ -z "$GOPATH" ]]; then
        export GOPATH=/home/d4ryus/go
fi

alias lock="sleep 1 && xset dpms force off && slock"
alias tm="tmux attach -t"
alias ts="tmux ls"
alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias dmesg="dmesg --color"
alias pg="ps -efa | grep "
alias upAur="update_Aur"
alias myip="curl http://myip.dnsomatic.com && echo ''"
alias tvim="nvim -c term"
alias et="emacsclient -a \"\" -c -t"
alias ec="emacsclient -a \"\" -c -n"

# git autocompletion
if [ -r ~/.git-completion-bash ]; then
        source ~/.git-completion-bash
fi

# wikidates setting
if [[ $(cat /proc/$PPID/status | head -1 | cut -f2) != "sshd" ]]; then
        if [ -d ~/.wikidates ]; then
                cat ~/.wikidates/$(date +%B_%d) | shuf -n 1
        fi
fi

# neovim terminal
if [ "$NVIM_LISTEN_ADDRESS" != "" ]; then
        if [[ $(type -P "$(which nvimex.py 2>>/dev/null)") ]];  then
                alias :="nvimex.py"
                alias vim="nvimex.py e"
                alias nvim="nvimex.py e"
        else
                echo "could not find nvimex.py in PATH. nvim aliases disabled."
        fi
fi

if [ -r ~/.bashrc.local ]; then
        source ~/.bashrc.local
fi

et-wrapper() {
        emacsclient -a "" -c -t -e "(progn (find-file \"$1\") (cd \"$PWD\"))"
}

# show 256 colors
show_colors() {
        for i in {0..255}
        do
                printf "\x1b[38;5;${i}mcolour${i}\n"
        done
}

# Extract Files
ext() {
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

cinst() {
        cower --download $1
        if [ ! $? -eq 0 ]; then
                return
        fi
        cd $1
        makepkg --syncdeps --install
        if [ -d ~/aurPackages/ ]; then
                cp *.pkg.tar.xz ~/aurPackages/
        fi
        cd ..
        rm -rf $1
}

update_Aur() {
        LOC="$(pwd)"
        TMP="/tmp/aurPackages"

        mkdir "$TMP" && cd "$TMP"

        cower --update --download
        find -name PKGBUILD -execdir makepkg --syncdeps --install \;
        if [ -d ~/aurPackages/ ]; then
                cp $TMP/*/*.pkg.tar.xz ~/aurPackages/
        fi

        cd "$LOC"

        echo "remove $TMP? (Y/n)"
        read answer
        if [[ $answer == "Y" || $answer == "" ]]; then
                rm --recursive --force "$TMP"
                return
        fi
}

con_wireless() {
        INTERFACE="wlp3s0"
        WPA_CONF="/etc/wpa_supplicant/wpa_supplicant-wlp3s0.conf"

        if [[ $EUID -ne 0 ]]; then
                echo "call me again when u got root!"
                return
        fi

        echo "setting $INTERFACE up"
        ip link set $INTERFACE up
        echo "starting wpa on $INTERFACE with $WPA_CONF"
        wpa_supplicant -i $INTERFACE -B -c $WPA_CONF
        echo "getting ip via dhcpcd"
        dhcpcd $INTERFACE
        echo "should be all done now."
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
        PS1='['
        # PS1 user color
        if [[ $EUID -ne 0 ]]; then
                # user = green
                PS1+='\[\033[0;32m\]'
        else
                # root = red
                PS1+='\[\033[0;31m\]'
        fi
        # user and current path
        PS1+='\h\[\033[0m\] \w'
        # use vcprompt if installed
        if command -v vcprompt > /dev/null 2>&1 ; then
                # space and yellow color
                PS1+='\[\033[0;33m\]'
                PS1+='$(vcprompt -f " %n:%b%u%m")'
                # end yellow color
                PS1+='\[\033[0m\]'
        fi
        PS1+='] '
}

set_ps1
