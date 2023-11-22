#!/bin/bash
current_directory=$(pwd)
mapfile -t program_directories < <(ls templates/)

overall_exit_code=0

for dir in "${program_directories[@]}"; do
    echo "==============================="
    echo "Testing $dir"
    echo "==============================="

    cd "templates/$dir" || exit 1
    if ! ./test.sh; then
        echo "[ERROR] Test for template $dir failed!"
        overall_exit_code=1
    fi
    cd "$current_directory" || exit 1

    echo "==============================="
    if [ "$dir" != "${program_directories[-1]}" ]; then
        echo
    fi
done

cd "$current_directory" || exit 1
exit $overall_exit_code
