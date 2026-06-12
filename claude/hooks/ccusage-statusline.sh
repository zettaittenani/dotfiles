#!/usr/bin/env bash
# Claude Code の statusLine 用ラッパ。ccusage statusline の出力をそのまま返す。
#
# settings.json に node バージョンや username を直書きすると別マシンへ移植できない
# ため、解決をこのスクリプトに閉じ込める。PATH の先頭に下記を積んで `ccusage` を探す:
#   1. fnm の default alias の bin (主環境。node バージョンに非依存で追従する)
#   2. ~/.local/bin / Homebrew (npx フォールバック用に node を見つけられるように)
# いずれも無ければ npx で実行 (どの node でも動くが起動が遅い)。
set -uo pipefail

export PATH="$HOME/.local/share/fnm/aliases/default/bin:$HOME/.local/bin:/opt/homebrew/bin:$PATH"

# Claude Code が stdin でセッション JSON を渡してくるので受け取って中継する
input="$(cat)"

if command -v ccusage >/dev/null 2>&1; then
  printf '%s' "$input" | ccusage statusline
elif command -v npx >/dev/null 2>&1; then
  printf '%s' "$input" | npx -y ccusage statusline
fi
# ccusage も npx も無い環境では何も出さない (statusline が空になるだけ)
exit 0
