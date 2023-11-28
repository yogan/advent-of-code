#!/bin/bash
current_directory=$(pwd)
mapfile -t template_dirs < <(find templates/ -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort)

overall_exit_code=0

for dir in "${template_dirs[@]}"; do
    echo "==============================================================================="
    echo "Testing template for $dir"
    echo "==============================================================================="

    cd "templates/$dir" || exit 1
    if ! ./test-ci.sh; then
        echo "[ERROR] Test for template $dir failed!"
        overall_exit_code=1
    fi
    cd "$current_directory" || exit 1

    echo "==============================================================================="
    if [ "$dir" != "${template_dirs[-1]}" ]; then
        echo
    fi
done

cd "$current_directory" || exit 1
exit $overall_exit_code
