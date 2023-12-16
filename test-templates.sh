#!/bin/bash
current_directory=$(pwd)
overall_exit_code=0

mapfile -t template_dirs < <(find templates/ -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort)

for dir in "${template_dirs[@]}"; do
    echo -n "Testing template for $dir… "
    cd "templates/$dir" || exit 1
    start=$(date +%s.%N)
    if ! ./test-ci.sh; then
        echo "FAILED"
        overall_exit_code=1
    else
        end=$(date +%s.%N)
        runtime=$(printf "%.0f" "$(echo "($end - $start) * 1000" | bc)")
        echo "OK ($runtime ms)"
    fi
    cd "$current_directory" || exit 1
done

cd "$current_directory" || exit 1
exit $overall_exit_code
