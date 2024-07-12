#!/bin/sh
set -x
set -e pipefail

if command -v fd >/dev/null; then
    fd_bin=fd
else
    # Docker image is using fd-find deb from Ubuntu, binary is named fdfind
    fd_bin=fdfind
fi

$fd_bin README.md | xargs npx markdown-link-check --quiet --verbose \
    --config .markdown-link-check.config.json
