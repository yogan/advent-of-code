#!/bin/bash
sed -e '/^\("\|:\?$\)/d' <aoc.vim >aoc.stripped.vim
vim --clean -s "aoc.stripped.vim" 1>/dev/null 2> \
    >(grep -v "Vim: Warning: Output is not to a terminal" >&2)
cat out
rm -f aoc.stripped.vim out
