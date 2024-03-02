#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '(
    ("" "
    let f = proc(x:int) ( x - 11 )
in ( f   ( f   7 ) )")
  ("" "
( proc(f:(int -> int)) ( f   ( f   77 ) )   proc(x:int) ( x - 11 ) )")
  ("" "
let x = 200
in let f = proc(z:int) ( z - x )
   in let x = 100
      in let g = proc(z:int) ( z - x )
         in ( ( f   1 ) - ( g   1 ) )")
  ("" "
letrec int double(x:int) = if zero?(x)
                           then 0
                           else ( ( double   ( x - 1 ) ) - -2 )
in ( double   6 )")
  ("" "
let f = proc(x:int) 10
in letrec int f(x:int) = if zero?(x) then 1 else ( f   ( x - 1 ) )
   in ( f   3 )")
  ("" "
let f = proc(x:int) ( x - 11 )
in ( f   zero?(7) )")
  ("" "
let twice = proc(f:(int -> int)) proc(x:int) ( f   ( f   x ) )
in twice")
  ("" "
let twice = proc(f:(int -> int)) proc(x:int) ( f   ( f   x ) )
in ( twice   proc(x:int) x )")
  ("" "
proc(x:int) zero?(x)")
  ))