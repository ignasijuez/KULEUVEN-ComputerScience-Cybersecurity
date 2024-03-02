#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '(
    ("Simple expression continuations" "
( 10 - ( 2 - 4 ) )")
    ("With changes in environment" "
let a=10 in let b = 5 in ((a - 1) - b)")
    ("Environment shrinks on popping of the stack" "
( let a=10 in let b = 5 in ((a - 1) - b) - 1)")
    ("a deep stack" "
letrec double(x) = if zero?(x) then 0 else ( ( double   ( x - 1 ) ) - -2 )
in ( double   6 )")
  ))