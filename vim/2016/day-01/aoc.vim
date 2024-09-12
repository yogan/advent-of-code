" ----------------------------------- input ------------------------------------

" read input and remove blank first line
:r input.txt|1d

" ----------------------------------- part 1 -----------------------------------

" put directions on separate lines, move back to first line
:%s/, /\r/g|0

" get number of lines
:let n = line('$')

" add start position (0, 0) and initial orientation (north → dx = 0, dy = -1)
:s/^/0 0;0 -1

" we are now starting to build a huge macro, therefore a lot of :let commands…
" eventually we'll concatenate all of them, using \r for <enter>

" derive new orientation from current one and L/R rotation
:let r1 = ":s/;0 -1L/;-1 0;/e"
:let r2 = ":s/;1 0L/;0 -1;/e"
:let r3 = ":s/;0 1L/;1 0;/e"
:let r4 = ":s/;-1 0L/;0 1;/e"
:let r5 = ":s/;0 -1R/;1 0;/e"
:let r6 = ":s/;1 0R/;0 1;/e"
:let r7 = ":s/;0 1R/;-1 0;/e"
:let r8 = ":s/;-1 0R/;0 -1;/e"
:let rot = r1."\r".r2."\r".r3."\r".r4."\r".r5."\r".r6."\r".r7."\r".r8."\r"

" we now have a line like this: px py;dx dy;steps
" px/py is the current position, dx/dy is the orientation, steps is the distance
" new position will be: px+dx*steps py+dy*steps
" for some reason, we need an eval for the y coordinate…
" (probably \= stops evaluating after concatenating a space)
:let c1 = ":silent! s/\\v(-?\\d+) (-?\\d+);(-?\\d+) (-?\\d+);(\\d+)/"
:let c2 = "\\=submatch(1)+submatch(3)*submatch(5).' '."
:let c3 = "eval(submatch(2)+submatch(4)*submatch(5))"
:let c4 = ".';'.submatch(3).' '.submatch(4)"
:let calc = c1 . c2 . c3 . c4 . "\r"

" do the rotation, calculate new position, join next line
:let @q = rot . calc . "gJ0"

" repeat the macro for each line (n holds number of lines)
" for some obscure reason, we get a "Press ENTER or type command to continue"
" message, which we are only able to get rid of with this unholy combination
" of :silent! and :redraw…
:silent! exe "norm " . n . "@q:redraw\r"

" we have our final position and orientation, like this: px py;dx dy
" the solution is the Manhattan distance: abs(px)+abs(py):
:s/\v(-?\d+) (-?\d+).*/\=abs(submatch(1))+abs(submatch(2))/

" delete part 1 result into register a
:norm "add

" ----------------------------------- part 2 -----------------------------------

" For part 2, we need to find the first position that is visited twice.
" Time to throw in some Vimscript…
" This function could also easily solve part 1 (just don't return when a
" duplicate position is found), but let's keep the initial part 1 solution that
" is based purely on cursed regexes.
:function Part2()
:   let dir = [0, -1]
:   let pos = [0, 0]
:   let vis = {}
:   for cmd in split(getline(1), ', ')
:       let dir = [dir[1], -dir[0]]
:       if cmd[0] == 'R'
:           let dir = [-dir[0], -dir[1]]
:       endif
:       for _ in range(str2nr(cmd[1:]))
:           let pos = [pos[0] + dir[0], pos[1] + dir[1]]
:           let key = pos[0] . '/' . pos[1]
:           if has_key(vis, key)
:               put =abs(pos[0]) + abs(pos[1])
:               return
:           endif
:           let vis[key] = 1
:       endfor
:   endfor
:endfunction

:r input.txt|1d
:call Part2()|1d

" ----------------------------------- output -----------------------------------

" restore part 1 result
:pu! a

" kthxbye
:x! out
