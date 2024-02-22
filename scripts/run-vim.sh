#!/bin/sh
commented=$(find . -name "*.commented.vim")
plain=$(echo "$commented" | sed 's/\.commented//g')
sed -e '/^\("\|:\?$\)/d' <"$commented" >"$plain"
vim --clean -s "$plain"
cat out
