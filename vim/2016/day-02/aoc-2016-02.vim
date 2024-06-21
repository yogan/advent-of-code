:function Unnormal(line)
:   for c in split(a:line, '\zs')
:       exe 'norm ' . c
:   endfor
:endfunction
:function Part1()
:   r input.txt|1d
:   silent %s/.*/\=tr(submatch(0), 'LDUR', 'hjkl')/g
:   let lines = getline(1, '$')
:   exe "norm cgg123\r456\r789\<esc>kh"
:   let result = ''
:   for line in lines
:       call Unnormal(line)
:       let result .= getline('.')[col('.') - 1]
:   endfor
:   %d
:   return result
:endfunction
:let p1 = Part1()
:function Part2()
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
:s/.*/\=p1 . "\r" . p2
:x! out
