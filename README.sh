#! /bin/bash
# to link everything just execute this README file with path to dotfiles repo
# for example: './README.sh /home/d4ryus/.dotfiles'

DOTFILES="zshrc xresources xinitrc xbindkeysrc vimrc vim tmux.conf gtkrc-2.0 \
          gitconfig eclimrc bashrc pentadactylrc muttrc mutt irssi"

for i in $(DOTFILES); do
        if [ -a $HOME/.$i ]; then
                mv $HOME/.$(i) $HOME/.$(i).backup;
        fi
        ln -s $1/$(i) $HOME/.$(i)
done
