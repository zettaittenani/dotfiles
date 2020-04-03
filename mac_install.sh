#!/bin/bash
set -ex

# Mac の開発環境セットアップスクリプト
# リラン可

# install homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# install tmux
brew install tmux

# install Emacs (with cocoa)
brew cask install emacs
git clone git@github.com:syohex/emacs-digdag-mode.git ~/.emacs.d/github.com/
install ./.emacs.d/init.el ~/.emacs.d/

# settings
install ./.zshrc ~/
install ./tmux/2.9/.tmux.conf ~/
install ./.vimrc ~/

# NeoBundle (for Vim) install
git clone git://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim

# Docker settings
install ./.docker/daemon.json ~/.docker
