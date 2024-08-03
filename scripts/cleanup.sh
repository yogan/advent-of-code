#!/bin/sh
script_dir=$(realpath "$(dirname "$0")")

echo "Free disk space before cleanup:"
df -h | grep root | awk '{print $4}'
echo

if [ -d "$HOME/.stack" ]; then
    echo "-----------------------------------------------------------"
    echo "Cleaning up Haskell/Stack ~/.stack directory"
    du -sh "$HOME/.stack"
    "$script_dir/haskell-cleanup.sh"
    echo
    echo "-----------------------------------------------------------"
    echo "Free disk space after Haskell/Stack cleanup:"
    df -h | grep root | awk '{print $4}'
    echo
fi

echo "-----------------------------------------------------------"
echo "Clean up build artifacts"
git clean -fdx .
echo

echo "-----------------------------------------------------------"
echo "Free disk space after git cleanup:"
df -h | grep root | awk '{print $4}'
