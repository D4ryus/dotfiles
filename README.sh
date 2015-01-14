#! /bin/bash
# to link everything just execute this README file

IGNORE="README.sh dwm_6.0.diff"

DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

for i in $(ls $DIR); do
        if [[ $IGNORE =~ .*$i.* ]]; then
                continue;
        fi
        if [ -a $HOME/.$i ]; then
                mv $HOME/.$i $HOME/.$i.backup;
        fi
        ln -s $DIR/$i $HOME/.$i
done
