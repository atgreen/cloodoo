#!/usr/bin/env bash
# Check that the cloodoo(1) man page lists the same top-level commands as
# `cloodoo --help`, catching drift when commands are added or renamed
# (cloodoo-3n7).
#
# Usage: releng/check-man-sync.sh [path-to-cloodoo] [path-to-man-page]
set -euo pipefail

BIN="${1:-./cloodoo}"
MAN="${2:-releng/cloodoo.1}"

# Top-level command names from --help: indented rows in the COMMANDS section.
# "setup-extension, install-native-host" style rows contribute each alias.
help_cmds=$("$BIN" --help \
  | awk '/^COMMANDS:/{f=1; next} /^[A-Z]/{f=0} f && NF {print $0}' \
  | sed 's/^ *//' \
  | awk '{gsub(",", "", $1); print $1; if ($2 ~ /^[a-z-]+$/ && $0 ~ /,/) print $2}' \
  | sort -u)

# Command names from the man page: the first word of every .B line in the
# COMMANDS section that looks like a bare command (options like \-\-pdf and
# single-letter flags are .B lines too — skip them).
man_cmds=$(awk '/^\.SH COMMANDS/{f=1; next} /^\.SH/{f=0} f && /^\.B /{print $2}' "$MAN" \
  | sed 's/\\f[BIR]//g' \
  | grep -E '^[a-z][a-z-]*$' \
  | sort -u)

missing_from_man=$(comm -23 <(echo "$help_cmds") <(echo "$man_cmds"))
missing_from_help=$(comm -13 <(echo "$help_cmds") <(echo "$man_cmds"))

status=0
if [ -n "$missing_from_man" ]; then
  echo "Commands in --help but missing from $MAN:" >&2
  echo "$missing_from_man" | sed 's/^/  /' >&2
  status=1
fi
if [ -n "$missing_from_help" ]; then
  echo "Commands documented in $MAN but not in --help:" >&2
  echo "$missing_from_help" | sed 's/^/  /' >&2
  status=1
fi

if [ "$status" -eq 0 ]; then
  echo "man page and --help agree ($(echo "$help_cmds" | wc -l) commands)"
fi
exit "$status"
