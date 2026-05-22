#!/bin/bash
set -ex

# Mac の開発環境セットアップスクリプト
# リラン可

# install homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# install tmux
brew install tmux

# install Ghostty and its font
brew install --cask ghostty
brew install --cask font-jetbrains-mono-nerd-font

# install Emacs (with cocoa)
brew install --cask emacs
git clone git@github.com:syohex/emacs-digdag-mode.git ~/.emacs.d/github.com/
install ./.emacs.d/init.el ~/.emacs.d/

# settings
install ./.zshrc ~/
install ./tmux/2.9/.tmux.conf ~/
install ./.vimrc ~/
install -D ./ghostty/config ~/.config/ghostty/config

# NeoBundle (for Vim) install
git clone https://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim

# Docker settings
install ./.docker/config.json ~/.docker

# Other settings
brew install coreutils
brew install gawk
brew link --overwrite gawk
