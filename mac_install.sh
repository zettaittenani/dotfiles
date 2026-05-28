#!/bin/bash
set -ex

# Mac の開発環境セットアップスクリプト
# リラン可

# install homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# install tmux
brew install tmux

# install Ghostty (font is macOS built-in Monaco)
brew install --cask ghostty

# install Emacs (with cocoa)
brew install --cask emacs
git clone git@github.com:syohex/emacs-digdag-mode.git ~/.emacs.d/github.com/
install ./.emacs.d/init.el ~/.emacs.d/

# settings
install ./.zshrc ~/
install ./tmux/2.9/.tmux.conf ~/
install ./.vimrc ~/
install -D ./ghostty/config ~/.config/ghostty/config

# vim-plug (for Vim) install. Run `:PlugInstall` inside vim afterwards.
sh -c 'curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

# Docker settings
install ./.docker/config.json ~/.docker

# Other settings
brew install coreutils
brew install gawk
brew link --overwrite gawk

# Dev runtime managers and tooling used by ~/.zshrc
brew install direnv
brew install jenv
brew install fnm
brew install mise
brew install pnpm

# Tmux companion tooling
# - workmux: git worktree + tmux window orchestrator
brew install raine/workmux/workmux

# terminal-notifier: クリック可能な macOS 通知 (Claude Code hook から呼ぶ)
brew install terminal-notifier

# Claude Code 用 hook script (Stop 等の通知でクリック→該当 pane へジャンプ)
install -D ./claude/hooks/click-to-pane-notify.sh ~/.claude/hooks/click-to-pane-notify.sh

# Claude Code settings.json (テンプレ内 __HOME__ を実 $HOME に展開して配置)
# 既存ファイルがあれば .bak に退避してから上書き
mkdir -p ~/.claude
[ -f ~/.claude/settings.json ] && cp ~/.claude/settings.json ~/.claude/settings.json.bak
sed "s|__HOME__|${HOME}|g" ./claude/settings.json > ~/.claude/settings.json

# Rust toolchain (rustup manages stable/nightly toolchains).
# After install, `rustup default stable` to pull rustc/cargo.
brew install rustup
"$(brew --prefix)/opt/rustup/bin/rustup" default stable
