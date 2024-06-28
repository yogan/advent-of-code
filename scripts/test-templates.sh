#!/bin/bash
exit_code=0
cwd=$(pwd)

# Haskell fix
if [ -n "$GITHUB_ACTIONS" ]; then
    # When running in GitHub Actions, $HOME is /home/github, which is owned by
    # uid 1001 / gid 127. We are running as root though, and stack does not like
    # to create its ~/.stack directory in a directory owned by a different user.
	# We can be quicker though, by just creating the directory ourselves. After
	# that, stack will be happy to install its stuff there.
	echo "Running in GitHub Actions, creating ~/.stack directory as root"
	mkdir -p "$HOME/.stack"
fi

execute_command() {
    command="$1"
    log_file="$2"
    $command >"$log_file" 2>&1
    code=$?
    if [ $code -ne 0 ]; then
        echo "Error running \"$command\". Printing log file \"$log_file\":" >&2
        echo "------------------------------------------------------------" >&2
        cat "$log_file" >&2
        rm -f "$log_file"
        return $code
    fi
    rm -f "$log_file"
}

mapfile -t template_dirs < <(find templates/ -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort)

for dir in "${template_dirs[@]}"; do
    cd "templates/$dir" || exit 1

    lang=$(<.language)
    echo -n "Testing $lang template… "

    # optional build step
    if [ -x "build-ci.sh" ]; then
        start=$(date +%s.%N)
        if ! execute_command "./build-ci.sh" "build.log"; then
            echo "FAILED (build)"
            exit_code=1
            cd "$cwd" || exit 1
            continue
        fi
        end=$(date +%s.%N)
        build_runtime=$(printf "%.0f" "$(echo "($end - $start) * 1000" | bc)")
        if [ "$build_runtime" -ge 1000 ]; then
            build_runtime=$(printf "%.1f s" "$(echo "$build_runtime / 1000" | bc -l)")
        else
            build_runtime="${build_runtime} ms"
        fi
    else
        build_runtime=""
    fi

    start=$(date +%s.%N)
    if ! execute_command "./test-ci.sh" "test.log"; then
        echo "FAILED"
        exit_code=1
    else
        end=$(date +%s.%N)
        runtime=$(printf "%.0f" "$(echo "($end - $start) * 1000" | bc)")
        if [ "$runtime" -ge 1000 ]; then
            runtime=$(printf "%.1f s" "$(echo "$runtime / 1000" | bc -l)")
        else
            runtime="${runtime} ms"
        fi
        if [ -n "$build_runtime" ]; then
            runtime_stats="build: ${build_runtime}, run: ${runtime}"
        else
            runtime_stats="${runtime}"
        fi
        echo "OK ($runtime_stats)"
    fi

    cd "$cwd" || exit 1
done

# Haskell fix
if [ -n "$GITHUB_ACTIONS" ]; then
	echo "Running in GitHub Actions, cleaning up ~/.stack directory"
	rm -rf "$HOME/.stack"
fi

cd "$cwd" || exit 1
exit $exit_code
