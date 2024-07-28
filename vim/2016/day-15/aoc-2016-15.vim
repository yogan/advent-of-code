:function ReadInput()
:   r input.txt | 1d | let lines = getline(1, '$') | 1,$d
:   let time = 0 | let as = [] | let ns = []
:   for line in lines
:       let time += 1
:       let parts     = split(line, ' ')
:       let positions = str2nr(parts[3])
:       let start     = str2nr(parts[11][:-2]) + time
:       call add(as, (9 * positions - start) % positions) " * 9 to make positive
:       call add(ns, positions)
:   endfor
:   return [as, ns]
:endfunction
:function ChineseRemainderTheorem(as, ns)
:   let prod = 1 | for n in a:ns | let prod *= n | endfor
:   let sum = 0
:   for i in range(len(a:as))
:       let p = prod / a:ns[i]
:       let sum += a:as[i] * p * ModularMultiplicativeInverse(p, a:ns[i])
:   endfor
:   return sum % prod
:endfunction
:function ModularMultiplicativeInverse(a, mod)
:   let b = a:a % a:mod
:   for x in range(1, a:mod)
:       if (b * x) % a:mod == 1 | return x | endif
:   endfor
:   return 1
:endfunction
:let input = ReadInput()
:pu =ChineseRemainderTheorem(input[0], input[1])
:pu =ChineseRemainderTheorem(input[0] + [11-7], input[1] + [11])
:1d | x! out
