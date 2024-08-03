#!/bin/sh
echo
echo "Free disk space before cleanup:"
df -h | grep root | awk '{print $4}'
echo "-----------------------------------------------------------"
echo
echo "Clean up build artifacts"
git clean -fdx .
echo "-----------------------------------------------------------"
echo
echo "Free disk space after git cleanup:"
df -h | grep root | awk '{print $4}'
