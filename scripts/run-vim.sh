#!/bin/bash
commented=$(find . -name "*.commented.vim")
plain=${commented//\.commented/}
sed -e '/^\("\|:\?$\)/d' <"$commented" >"$plain"
vim --clean -s "$plain" 2> >(grep -v "Vim: Warning: Output is not to a terminal" >&2)
