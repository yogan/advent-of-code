" ----------------------------------- input ------------------------------------

" read input and remove blank first line
:r input.txt|1d

" ----------------------------------- part 1 -----------------------------------

" put directions on separate lines, move back to first line
:%s/, /\r/g|0

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
:let calc = c1.c2.c3.c4."\r"

" do the rotation, calculate new position, join next line
:let @q = rot . calc . "gJ0"

" repeat the macro 999 times to process all input commands
" for some obscure reason, we get a "Press ENTER or type command to continue"
" message, which we are only able to get rid of with this unholy combination
" of :silent! and :redraw…
:silent! exe "norm 999@q:redraw\r"

" we have our final position and orientation, like this: px py;dx dy
" the solution is the Manhattan distance: abs(px)+abs(py):
:s/\v(-?\d+) (-?\d+).*/\=abs(submatch(1))+abs(submatch(2))/

" ----------------------------------- output -----------------------------------

" kthxbye
:x! out
