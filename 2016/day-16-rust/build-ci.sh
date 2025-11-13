#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

RUSTUP_LOG="rustup_log.txt"
BUILD_LOG="build_log.txt"

execute_command() {
    command="$1"
    log_file="$2"
    $command >"$log_file" 2>&1
    exit_code=$?
    if [ $exit_code -ne 0 ]; then
        echo "Error running \"$command\". Printing log file \"$log_file\":" >&2
        echo "------------------------------------------------------------" >&2
        cat "$log_file" >&2
        rm -f "$log_file"
        exit $exit_code
    fi
}

# Sometimes rustup complains that no default toolchain is configured.
# Could be caused by network issues, see: https://stackoverflow.com/a/46864309
execute_command "rustup default stable" "$RUSTUP_LOG"

execute_command "cargo build" "$BUILD_LOG"

rm -f "$RUSTUP_LOG" "$BUILD_LOG"
