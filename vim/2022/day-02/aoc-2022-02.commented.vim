" read input data
:r input.txt
" delete blank first line
:1d
" yank lines into register a (for part 2)
:norm vip"ay

" ---- part 1 ----

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

" define a macro v that:
" - joins lines together to form a single sum
" - to evaluates the sum via the expression register
:let @v = "vipgJ$x0c$\<C-r>=\<C-r>\"\<cr>"
" execute the macro
:norm @v

" ---- part 2 ----

" paste original lines from register a
:norm "ap

" nine possible games:
" A X = rock     lose = 1 3 = 0 + 3 = 3
" A Y = rock     draw = 1 1 = 3 + 1 = 4
" A Z = rock     win  = 1 2 = 6 + 2 = 8
" B X = paper    lose = 2 1 = 0 + 1 = 1
" B Y = paper    draw = 2 2 = 3 + 2 = 5
" B Z = paper    win  = 2 3 = 6 + 3 = 9
" C X = scissors lose = 3 2 = 0 + 2 = 2
" C Y = scissors draw = 3 3 = 3 + 3 = 6
" C Z = scissors win  = 3 1 = 6 + 1 = 7
:%s/A X/3+/g
:%s/A Y/4+/g
:%s/A Z/8+/g
:%s/B X/1+/g
:%s/B Y/5+/g
:%s/B Z/9+/g
:%s/C X/2+/g
:%s/C Y/6+/g
:%s/C Z/7+/g

" execute the sum evaluation macro from part 1
:norm @v

" remove blank lines
:g/^$/d
" write result to output file
:x! out
