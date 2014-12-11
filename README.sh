#! /bin/bash
# to link everything just execute this README file with path to dotfiles repo
# for example: './README.sh /home/d4ryus/.dotfiles'
# its important to specify the full path to repo, otherwise links will not work

for i in $(ls $1); do
        if [ -a $HOME/.$i ]; then
                mv $HOME/.$i $HOME/.$i.backup;
        fi
        ln -s $1/$i $HOME/.$i
done
