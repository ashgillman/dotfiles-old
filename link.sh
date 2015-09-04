#!/bin/bash

# Assumes $PWD is the dotfiles root

ln -s $PWD/aliases   ~/.aliases
ln -s $PWD/bashrc    ~/.bashrc
ln -s $PWD/emacs.d   ~/.emacs.d
ln -s $PWD/oh-my-zsh ~/.oh-my-zsh
ln -s $PWD/vim       ~/.vim
ln -s $PWD/zshrc     ~/.zshrc
