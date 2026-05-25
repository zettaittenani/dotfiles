# dotfiles

Personal dotfiles for macOS (Apple Silicon).

## Install

```sh
git clone git@github.com:zettaittenani/dotfiles.git ~/dotfiles
cd ~/dotfiles
bash mac_install.sh
```

`mac_install.sh` is idempotent — re-running it is safe.

## What gets installed

- Homebrew + tmux + Emacs (cask) + coreutils + gawk
- Ghostty + Hack Nerd Font (cask)
- Config files:
  - `~/.zshrc`
  - `~/.vimrc`
  - `~/.tmux.conf` (from `tmux/2.9/`)
  - `~/.config/ghostty/config`
  - `~/.docker/config.json`
- NeoBundle for Vim (under `~/.vim/bundle/`)
- `emacs-digdag-mode` (under `~/.emacs.d/`)

## Layout

```
.
├── .docker/        # Docker client config
├── .emacs.d/       # Emacs init.el
├── .vimrc
├── .zshrc
├── ghostty/        # → ~/.config/ghostty/
├── tmux/2.9/       # → ~/.tmux.conf (tmux 2.9+)
└── mac_install.sh
```
