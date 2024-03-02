#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '(
    ("" "
letrec double(x) = if zero?(x) then 0 else ( ( double   ( x - 1 ) ) - -2 )
in ( double   6 )")
    ("letrec is recursive" "
let f = proc(x) 10
in letrec f(x) = if zero?(x) then 1 else ( f   ( x - 1 ) )
   in ( f   3 )")
    ("" "
letrec mult(n) = proc(m) if zero?(n)
                         then 0
                         else ( m - ( 0 - ( ( mult   ( n - 1 ) )   m ) ) )
in letrec fac(n) = if zero?(n) then 1 else ( ( mult   n )   ( fac   ( n - 1 ) ) )
   in ( fac   50 )")
  ))