#!/bin/sh
set -e pipefail

if command -v fd >/dev/null; then
    fd README | xargs npx markdown-link-check --quiet --verbose
else
    # Docker image is using fd-find deb from Ubuntu, binary is named fdfind
    fdfind README | xargs npx markdown-link-check --quiet --verbose
fi
