#!/bin/sh
# When running in GitHub Actions, $HOME is /home/github, which is owned by
# uid 1001 / gid 127. We are running as root though, and stack does not like
# to create its ~/.stack directory in a directory owned by a different user.
# We can be quicker though, by just creating the directory ourselves. After
# that, stack will be happy to install its stuff there.
if [ -n "$GITHUB_RUN_ID" ] && [ ! -d "$HOME/.stack" ]; then
    mkdir -p "$HOME/.stack"
fi
