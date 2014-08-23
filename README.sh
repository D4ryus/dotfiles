#! /bin/bash
# to link everything just execute the README file with path to dotfiles repo

if [ -a $HOME/.zshrc         ]; then mv $HOME/.zshrc         $HOME/.zshrc.backup         ; fi && ln -s $1/zshrc         $HOME/.zshrc
if [ -a $HOME/.xresources    ]; then mv $HOME/.xresources    $HOME/.xresources.backup    ; fi && ln -s $1/xresources    $HOME/.xresources
if [ -a $HOME/.xinitrc       ]; then mv $HOME/.xinitrc       $HOME/.xinitrc.backup       ; fi && ln -s $1/xinitrc       $HOME/.xinitrc
if [ -a $HOME/.xbindkeysrc   ]; then mv $HOME/.xbindkeysrc   $HOME/.xbindkeysrc.backup   ; fi && ln -s $1/xbindkeysrc   $HOME/.xbindkeysrc
if [ -a $HOME/.vimrc         ]; then mv $HOME/.vimrc         $HOME/.vimrc.backup         ; fi && ln -s $1/vimrc         $HOME/.vimrc
if [ -a $HOME/.vim           ]; then mv $HOME/.vim           $HOME/.vim.backup           ; fi && ln -s $1/vim           $HOME/.vim
if [ -a $HOME/.tmux.conf     ]; then mv $HOME/.tmux.conf     $HOME/.tmux.conf.backup     ; fi && ln -s $1/tmux.conf     $HOME/.tmux.conf
if [ -a $HOME/.gtkrc-2.0     ]; then mv $HOME/.gtkrc-2.0     $HOME/.gtkrc-2.0.backup     ; fi && ln -s $1/gtkrc-2.0     $HOME/.gtkrc-2.0
if [ -a $HOME/.gitconfig     ]; then mv $HOME/.gitconfig     $HOME/.gitconfig.backup     ; fi && ln -s $1/gitconfig     $HOME/.gitconfig
if [ -a $HOME/.eclimrc       ]; then mv $HOME/.eclimrc       $HOME/.eclimrc.backup       ; fi && ln -s $1/eclimrc       $HOME/.eclimrc
if [ -a $HOME/.bashrc        ]; then mv $HOME/.bashrc        $HOME/.bashrc.backup        ; fi && ln -s $1/bashrc        $HOME/.bashrc
if [ -a $HOME/.bash_profile  ]; then mv $HOME/.bash_profile  $HOME/.bash_profile.backup  ; fi && ln -s $1/bashrc        $HOME/.bash_profile
if [ -a $HOME/.pentadactylrc ]; then mv $HOME/.pentadactylrc $HOME/.pentadactylrc.backup ; fi && ln -s $1/pentadactylrc $HOME/.pentadactylrc
if [ -a $HOME/.muttrc        ]; then mv $HOME/.muttrc        $HOME/.muttrc.backup        ; fi && ln -s $1/muttrc        $HOME/.muttrc
if [ -a $HOME/.mutt          ]; then mv $HOME/.mutt          $HOME/.mutt.backup          ; fi && ln -s $1/mutt          $HOME/.mutt
if [ -a $HOME/.irssi         ]; then mv $HOME/.irssi         $HOME/.irssi.backup         ; fi && ln -s $1/irssi         $HOME/.irssi
