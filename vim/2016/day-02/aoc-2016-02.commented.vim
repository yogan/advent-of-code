" ----------------------------------- part 1 -----------------------------------

" You'd think that we can just yank the movement commands of each line into a
" register and execute it as a macro, or :normal it, but no. When we are trying
" to do a movement that does not work (e.g. moving left in the first column),
" this is treated as an error and subsequent commands are aborted (see :help
" :normal).
" So we build our own function, called Unnormal, which executes each character
" as a single command.
:function Unnormal(line)
:   for c in split(a:line, '\zs')
:       exe 'norm ' . c
:   endfor
:endfunction

:function Part1()
"   read input and remove blank first line
:   r input.txt|1d
"   convert input to Vim movements (golfy way with the tr() function)
:   silent %s/.*/\=tr(submatch(0), 'LDUR', 'hjkl')/g
:   let lines = getline(1, '$')
"   delete input and create the keypad, kh to move cursor to the middle (5)
:   exe "norm cgg123\r456\r789\<esc>kh"
"   "execute" each line with our unnormal function and build the result
:   let result = ''
:   for line in lines
:       call Unnormal(line)
"       append digit under cursor to result
:       let result .= getline('.')[col('.') - 1]
:   endfor
"   clear the buffer
:   %d
:   return result
:endfunction

:let p1 = Part1()

" ----------------------------------- part 2 -----------------------------------

" Now we have to deal with this diamond-shaped keypad:
"
"              1
"            2 3 4
"          5 6 7 8 9
"            A B C
"              D
"
" There is no way to use Vim movements on that, so we'll solve it
" programmatically. Possible movements are put in a dictionary. When there is
" no entry for a position/movement pair, we stay at the current position.
:function Part2()
"   NOTE: dict is built entry by entry, as multiline commands broken with \
"         does work when using :source, but somehow breaks when calling Vim
"         with -s script.vim
:   let moves = {}
:   let moves.1 = {'D': '3'}
:   let moves.2 = {'R': '3', 'D': '6'}
:   let moves.3 = {'U': '1', 'R': '4', 'D': '7', 'L': '2'}
:   let moves.4 = {'L': '3', 'D': '8'}
:   let moves.5 = {'R': '6'}
:   let moves.6 = {'U': '2', 'R': '7', 'D': 'A', 'L': '5'}
:   let moves.7 = {'U': '3', 'R': '8', 'D': 'B', 'L': '6'}
:   let moves.8 = {'U': '4', 'R': '9', 'D': 'C', 'L': '7'}
:   let moves.9 = {'L': '8'}
:   let moves.A = {'U': '6', 'R': 'B'}
:   let moves.B = {'U': '7', 'R': 'C', 'D': 'D', 'L': 'A'}
:   let moves.C = {'U': '8', 'L': 'B'}
:   let moves.D = {'U': 'B'}
:   r input.txt|1d
:   let lines = getline(1, '$')
:   let result = ''
:   let pos = '5'
:   for line in lines
:       for c in split(line, '\zs')
:           if has_key(moves[pos], c)
:               let pos = moves[pos][c]
:           endif
:       endfor
:       let result .= pos
:   endfor
:   %d
:   return result
:endfunction

:let p2 = Part2()

" ----------------------------------- output -----------------------------------

" write results to buffer
:s/.*/\=p1 . "\r" . p2

" kthxbye
:x! out
