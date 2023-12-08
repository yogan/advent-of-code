#!/bin/bash
current_directory=$(pwd)
overall_exit_code=0

mapfile -t day_dirs < <(find . -mindepth 1 -maxdepth 1 -type d -iname 'day*' -printf '%f\n' | sort)

for dir in "${day_dirs[@]}"; do
    echo -n "$(basename "$current_directory")/$dir… "
    if [ ! -f "$dir/test-ci.sh" ]; then
        echo "SKIPPED (no test-ci.sh found)"
        continue
    fi

    cd "$dir" || exit 1
    if ! ./test-ci.sh; then
        echo "FAILED"
        overall_exit_code=1
    else
        echo "OK"
    fi
    cd "$current_directory" || exit 1
done

cd "$current_directory" || exit 1
exit $overall_exit_code
