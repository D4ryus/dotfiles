#! /bin/bash
# to link everything just execute this README file

DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

for i in $(ls $1); do
        if [ -a $HOME/.$i ]; then
                mv $HOME/.$i $HOME/.$i.backup;
        fi
        ln -s $DIR/$i $HOME/.$i
done
