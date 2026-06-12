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

# Claude Code 用 hook / statusLine スクリプトを実行権限付きで配置する。
# 注: GNU install の -D は BSD(macOS 標準) install では使えないため mkdir+cp で配置する。
mkdir -p ~/.claude/hooks
# Stop 等の通知でクリック→該当 pane へジャンプ
cp ./claude/hooks/click-to-pane-notify.sh ~/.claude/hooks/click-to-pane-notify.sh
# statusLine で ccusage のトークン/コストを常時表示
cp ./claude/hooks/ccusage-statusline.sh ~/.claude/hooks/ccusage-statusline.sh
chmod +x ~/.claude/hooks/click-to-pane-notify.sh ~/.claude/hooks/ccusage-statusline.sh
# ccusage 本体 (未導入なら入れる。statusLine の表示を高速化する。npm が無ければ
# ラッパが npx フォールバックするのでスキップしても動作はする)
command -v npm >/dev/null 2>&1 && npm install -g ccusage

# Claude Code settings.json (テンプレ内 __HOME__ を実 $HOME に展開して配置)
# 既存ファイルがあれば .bak に退避してから上書き
mkdir -p ~/.claude
[ -f ~/.claude/settings.json ] && cp ~/.claude/settings.json ~/.claude/settings.json.bak
sed "s|__HOME__|${HOME}|g" ./claude/settings.json > ~/.claude/settings.json

# Claude Code のグローバル指示ファイル (CLAUDE.md 及び @import されるドキュメント)を配置。
# 既存ファイルは .bak に退避してから上書きする。
for f in CLAUDE.md RTK.md PR_WORKFLOW.md; do
  [ -f ~/.claude/"$f" ] && cp ~/.claude/"$f" ~/.claude/"$f".bak
  cp ./claude/"$f" ~/.claude/"$f"
done

# Rust toolchain (rustup manages stable/nightly toolchains).
# After install, `rustup default stable` to pull rustc/cargo.
brew install rustup
"$(brew --prefix)/opt/rustup/bin/rustup" default stable

# rtk (https://github.com/rtk-ai/rtk): CLI proxy that compresses tool output
# before it reaches the LLM context. Install once, then wire it into each
# AI tool's config via `rtk init`. Idempotent — re-running just re-applies
# the hooks/config, so it's safe on a re-run of this script.
brew install rtk
rtk init -g                     # Claude Code / Copilot (default)
rtk init -g --gemini            # Gemini CLI
rtk init -g --codex             # Codex (OpenAI)
rtk init -g --agent cursor      # Cursor
rtk init -g --agent windsurf    # Windsurf
rtk init --agent cline          # Cline / Roo Code
rtk init --agent kilocode       # Kilo Code
rtk init --agent antigravity    # Google Antigravity
rtk init -g --agent pi          # Pi
rtk init --agent hermes         # Hermes

# Pi (https://pi.dev/): customizable coding agent. Extensions are managed via
# `pi install <source>`. The list below mirrors yuku's recommended set.
# `pi install` is idempotent — re-running just refreshes the package.
if command -v pi >/dev/null 2>&1; then
  # Anthropic Claude Pro/Max OAuth auth for Pi
  pi install npm:pi-anthropic-oauth
  # Codex-friendly tool set (diff-based edits, GPT image gen, web search)
  pi install npm:@howaboua/pi-codex-conversion
  # Claude Code style subagents
  pi install npm:@tintinweb/pi-subagents
  # rpiv: research -> plan -> implement -> validate workflow
  pi install npm:@juicesharp/rpiv-pi
  pi install npm:@juicesharp/rpiv-ask-user-question
  pi install npm:@juicesharp/rpiv-btw
  # Shopify's autonomous experiment loop driver
  pi install git:github.com/davebcn87/pi-autoresearch
fi
