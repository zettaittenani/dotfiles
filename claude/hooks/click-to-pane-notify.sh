#!/usr/bin/env bash
# クリック可能な macOS 通知を出して、クリック時にこの hook が動いた tmux pane に
# Ghostty 越しでジャンプさせる。
#
# Usage: click-to-pane-notify.sh <event-name>
#   event-name: stop | stop_failure | notification | permission_denied | ...
#
# stdin: Claude Code hook の JSON payload
set -uo pipefail

EVENT="${1:-stop}"
PAYLOAD=$(cat 2>/dev/null || true)

extract_field() {
  local field="$1"
  command -v jq >/dev/null 2>&1 || { echo ""; return; }
  jq -r --arg f "$field" '.[$f] // empty' <<<"$PAYLOAD" 2>/dev/null || echo ""
}

case "$EVENT" in
  stop)              DEFAULT_BODY="完了しました" ;;
  stop_failure)      DEFAULT_BODY="エラー終了" ;;
  notification)      DEFAULT_BODY="権限要求/確認待ち" ;;
  permission_denied) DEFAULT_BODY="権限が拒否されました" ;;
  *)                 DEFAULT_BODY="$EVENT" ;;
esac

BODY=$(extract_field message)
[[ -z "$BODY" ]] && BODY="$DEFAULT_BODY"

PANE_ID="${TMUX_PANE:-}"
TITLE="Claude Code"
SUBTITLE=""
EXECUTE_CMD=""

if [[ -n "$PANE_ID" ]] && command -v tmux >/dev/null 2>&1; then
  SESSION=$(tmux display-message -p -t "$PANE_ID" '#{session_name}' 2>/dev/null || echo "")
  WINDOW=$(tmux display-message -p -t "$PANE_ID" '#{window_index}' 2>/dev/null || echo "")
  SUBTITLE="${SESSION}:${WINDOW} (${PANE_ID})"
  # クリック時: Ghostty を前面に出してから、対象 session/window/pane を選択
  EXECUTE_CMD="/usr/bin/open -a Ghostty; /opt/homebrew/bin/tmux switch-client -t '${SESSION}'; /opt/homebrew/bin/tmux select-window -t '${PANE_ID}'; /opt/homebrew/bin/tmux select-pane -t '${PANE_ID}'"
fi

ARGS=(
  -title "$TITLE"
  -message "$BODY"
  -sound default
)
[[ -n "$SUBTITLE" ]] && ARGS+=( -subtitle "$SUBTITLE" )
[[ -n "$EXECUTE_CMD" ]] && ARGS+=( -execute "$EXECUTE_CMD" )

/opt/homebrew/bin/terminal-notifier "${ARGS[@]}" >/dev/null 2>&1 || true
exit 0
