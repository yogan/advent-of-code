#!/bin/bash
current_directory=$(pwd)
overall_exit_code=0
mapfile -t solution_dirs < <(find . -mindepth 1 -maxdepth 1 -type d -iname '20*' -printf '%f\n' | sort)

for dir in "${solution_dirs[@]}"; do
    if [ ! -f "$dir/test.sh" ]; then
        continue
    fi

    echo "==============================================================================="
    echo "Testing solutions for $dir"
    echo "==============================================================================="

    cd "$dir" || exit 1
    if ! ./test.sh; then
        overall_exit_code=1
    fi
    cd "$current_directory" || exit 1

    echo "==============================================================================="
    if [ "$dir" != "${solution_dirs[-1]}" ]; then
        echo
    fi
done

cd "$current_directory" || exit 1
exit $overall_exit_code
