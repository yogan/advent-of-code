" read data from input file
:r input.txt

" define macro q that:
" - selects the current paragraph (vip)
" - joins the lines with :j (J does not work for single line paragraphs)
" - goes down one line (which should be empty)
:let @q = "vip:j\<cr>j"
" execute the macro many times to apply this to all paragraphs
:norm 999@q

" get rid of empty lines
:g/^$/d
" transform lines to sums
:%s/ /+/g

" to to first line, first character
:norm gg0

" define macro v that:
" - yanks the line into the default register (")
" - we are still in insert mode from the c$, so <C-r> = is used to insert from
"   the expression register (=)
" - <C-r> " inserts the value of the default register (which is the line with
"   the sum)
" - evaluated sum ends up in the line
" - 0j goes to the beginning of the line and down one line to prepare for the
"   next iteration
:let @v = "c$\<C-r>=\<C-r>\"\<cr>\<Esc>0j"
" execute the macro many times to apply this to all lines
:norm 999@v

" sort numerically in reverse order
:%!sort -rn
" keep top 3 entries (4,$ is the range from line 4 to last line, d deletes)
:4,$d

" - ggyyp: duplicate the first line (biggest sum, result for part 1)
" - JJ: join lines 2 to 4 (three biggest sums)
:norm ggyypJJ
" again, tranform line to a sum
:s/ /+/g
" we can reuse the macro v from above to evaluate the sum
:norm @v
" sum of biggest three sums is the result for part 2 and ends up in line 2

" write results to output file
:x! out
