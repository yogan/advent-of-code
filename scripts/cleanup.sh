#!/bin/sh
echo "Free disk space before cleanup:"
df -h | grep root | awk '{print $4}'

if [ -d "$HOME/.stack" ]; then
    echo
    echo "-----------------------------------------------------------"
    echo "Cleaning up Haskell/Stack ~/.stack directory"
    du -sh "$HOME/.stack"
    rm -rf "$HOME/.stack"
    echo
    echo "Free disk space after Haskell/Stack cleanup:"
    df -h | grep root | awk '{print $4}'
fi

echo
echo "-----------------------------------------------------------"
echo "Clean up build artifacts"
git clean -fdx .

echo
echo "-----------------------------------------------------------"
echo "Free disk space after git cleanup:"
df -h | grep root | awk '{print $4}'
