#!/usr/bin/env bash
INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty' 2>/dev/null)

[ -z "$FILE_PATH" ] || [ ! -f "$FILE_PATH" ] && exit 0

if [[ "$FILE_PATH" == *.hs ]]; then
  fourmolu -i "$FILE_PATH" 2>/dev/null
elif [[ "$FILE_PATH" == *.ts ]] || [[ "$FILE_PATH" == *.tsx ]]; then
  npx biome format --write "$FILE_PATH" 2>/dev/null
fi

exit 0
