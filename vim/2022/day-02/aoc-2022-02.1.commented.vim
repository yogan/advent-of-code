" read input data
:r input.txt

" replace shapes with their values
" A=X=1 (rock)
" B=Y=2 (paper)
" C=Z=3 (scissors)
:%s/[AX]/1/g
:%s/[BY]/2/g
:%s/[CZ]/3/g

" two equal shapes: draw(3) + shape(\1)
:%s/\([123]\) \1/3+\1+/e

" rock vs paper: win(6) + paper(2)
:%s/1 2/6+2+/e
" rock vs scissors: loss(0) + scissors(3)
:%s/1 3/3+/e

" paper vs rock: loss(0) + rock(1)
:%s/2 1/1+/e
" paper vs scissors: win(6) + scissors(3)
:%s/2 3/6+3+/e

" scissors vs rock: win(6) + rock(1)
:%s/3 1/6+1+/e
" scissors vs paper: loss(0) + paper(2)
:%s/3 2/2+/e

" join lines together to form a single sum
:norm vipgJ$x0
" define a macro to evaluate the sum via the expression register
:let @v = "c$\<C-r>=\<C-r>\"\<cr>"
" execute the macro
:norm @v

" remove blank lines at top and bottom
:1d|:$d

" write result to output file
:x! out
