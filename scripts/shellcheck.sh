#!/bin/bash
set -euo pipefail

echo "🔍 Running shellcheck on all shell scripts..."

# Use fd to find all .sh files (respects .gitignore)
# (Ubuntu Docker image has fdfind instead of fd; locally both are the same)
script_files=$(fdfind --type f --extension sh)

if [[ -z "${script_files}" ]]; then
    echo "ℹ️ No shell scripts found to check"
    exit 0
fi

script_count=$(echo "${script_files}" | wc -l)
echo "📄 Checking ${script_count} shell scripts..."

# Run shellcheck on all found scripts
if echo "${script_files}" | xargs shellcheck; then
    echo "✅ All shell scripts passed shellcheck!"
    exit 0
else
    echo "❌ Some shell scripts have issues. See output above."
    exit 1
fi
