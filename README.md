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
- Ghostty (cask; font is macOS built-in Monaco)
- [`workmux`](https://github.com/raine/workmux) — git worktree + tmux window orchestrator
- [`tmux-agent-sidebar`](https://github.com/hiroppy/tmux-agent-sidebar) — sidebar that tracks Claude Code / Codex / OpenCode panes (installed via `tpm`)
- [`terminal-notifier`](https://github.com/julienXX/terminal-notifier) — used by the Claude Code hook below to fire clickable macOS notifications
- Config files:
  - `~/.zshrc`
  - `~/.vimrc`
  - `~/.tmux.conf` (from `tmux/2.9/`)
  - `~/.config/ghostty/config`
  - `~/.docker/config.json`
  - `~/.claude/hooks/click-to-pane-notify.sh` (clickable notification → jumps Ghostty to the originating tmux pane)

## Claude Code hooks (manual wiring)

`mac_install.sh` drops the script in `~/.claude/hooks/`, but `~/.claude/settings.json` is hand-edited to wire it up. Add the following under `hooks`:

```json
"Stop":              [{ "matcher": "", "hooks": [{ "type": "command", "command": "bash ~/.claude/hooks/click-to-pane-notify.sh stop" }] }],
"Notification":      [{ "matcher": "", "hooks": [{ "type": "command", "command": "bash ~/.claude/hooks/click-to-pane-notify.sh notification" }] }],
"StopFailure":       [{ "matcher": "", "hooks": [{ "type": "command", "command": "bash ~/.claude/hooks/click-to-pane-notify.sh stop_failure" }] }],
"PermissionDenied":  [{ "matcher": "", "hooks": [{ "type": "command", "command": "bash ~/.claude/hooks/click-to-pane-notify.sh permission_denied" }] }]
```

First-run notes:

- macOS shows a notification permission prompt the first time `terminal-notifier` fires. Allow it under System Settings → Notifications.
- `tmux-agent-sidebar` ships its own desktop notifications; the dotfiles `.tmux.conf` sets `@sidebar_notifications off` so this hook is the only source.
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
