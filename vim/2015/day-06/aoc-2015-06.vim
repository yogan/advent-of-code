:r input.txt|1d
:let l = line('$')
:%s/\v^(.*) (\d+),(\d+) through (\d+),(\d+)$/\="`m" . (submatch(2) - 1) . "j" . (submatch(3) - 1) . "l\<c-v>" . (submatch(4) - submatch(2)) . "j" . (submatch(5) - submatch(3)) . "l" . (submatch(1) == "turn on" ? "rX" : (submatch(1) == "turn off" ? "rx" : "~"))
:%s/\v([^0-9])0[jl]/\1/g
:exe "norm Go\<cr>\<esc>1000ix\<esc>yy999p(mm"
:let @q = "gg\"ad$dd@a"
:exe "norm " . l . "@q"
:1d
:norm vipgJ
:s/x//g
:s/.*/\=col('$') - 1
:d f
:function Part2()
:   r input.txt|1d
:   let row = []
:   for _ in range(1000)
:       call add(row, 0)
:   endfor
:   let grid = []
:   for _ in range(1000)
:       call add(grid, copy(row))
:   endfor
:   for i in range(1, line('$'))
:       let parts = split(getline(i))
:       if parts[0] == "turn"
:           let op = parts[1] == "on" ? 1 : -1
:           let start = split(parts[2], ",")
:           let end = split(parts[4], ",")
:           for x in range(start[0], end[0])
:               for y in range(start[1], end[1])
:                   let grid[x][y] = max([0, grid[x][y] + op])
:               endfor
:           endfor
:       elseif parts[0] == "toggle"
:           let start = split(parts[1], ",")
:           let end = split(parts[3], ",")
:           for x in range(start[0], end[0])
:               for y in range(start[1], end[1])
:                   let grid[x][y] += 2
:               endfor
:           endfor
:       endif
:   endfor
:   let sum = 0
:   for row in grid
:       for val in row
:           let sum += val
:       endfor
:   endfor
:   pu! =sum
:   2,$d
:endfunction
:call Part2()
:pu! f
:x! out
