#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '(("" "
let f = proc(x) ( x - 11 )
in ( f   ( f   77 ) )")
  ("" "
( proc(f) ( f   ( f   77 ) )   proc(x) ( x - 11 ) )")
  ("proc-val is more than the code" "
let x = 200
in let f = proc(z) ( z - x )
   in let x = 100
      in let g = proc(z) ( z - x )
         in ( ( f   1 ) - ( g   1 ) )")
  ("lexical scoping" "
let f = proc(w) let x = 100
                in proc(z) ( z - x )
in let x = 200
   in ( ( f   0 )   0 )")
  ("let is not recursive" "
let f = proc(x) 10
in let f = proc(x) if zero?(x) then 1 else ( f   ( x - 1 ) )
   in ( f   3 )")
  ("edit for infinite loop" "
proc(dummy) ( proc(x) ( x   x )   proc(x) ( x   x ) )")
  ))