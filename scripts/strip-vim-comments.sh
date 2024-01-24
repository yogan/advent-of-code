#!/bin/sh
in="$1"
out=$(echo "$1" | sed -e 's/\.commented//')
sed -e '/^\("\|$\)/d' <"$in" >"$out"
