#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '(
    (""
     "2")
    (""
     "( 2 - 4 )")
    (""
     "( 10 - ( 2 - 4 ) )")
    (""
     "x")
    (""
     "( x - 4 )")
    (""
     "
let x = 0
in ( x - 4 )
     ")
    ("nested binding"
     "
let x = 0
in let x = 2
   in x
     ")
    ("non-existing var"
     "xx")
    ("let is non-recursive"
     "
let y = 0
  in let y = ( y - 1 )
    in y
    ")
    (""
     "zero?(10)")
    ))