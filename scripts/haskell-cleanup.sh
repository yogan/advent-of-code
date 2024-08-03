#!/bin/sh
if [ -n "$GITHUB_RUN_ID" ] && [ -d "$HOME/.stack" ]; then
    rm -rf "$HOME/.stack"
fi
