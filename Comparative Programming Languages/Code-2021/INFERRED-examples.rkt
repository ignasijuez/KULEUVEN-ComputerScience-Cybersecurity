#lang racket
(provide (all-defined-out))
(define examples-with-notes
  '(
    ("" "
      ( proc(f:?) ( f   ( f   77 ) )   proc(x:?) ( x - 11 ) )")
    ("" "      let x = 200
      in let f = proc(z:?) ( z - x )
         in let x = 100
            in let g = proc(z:?) ( z - x )
               in ( ( f   1 ) - ( g   1 ) )")
    ("" "      letrec ? double(x:?) = if zero?(x) then 0 else ( ( double   ( x - 1 ) ) - -2 )
      in ( double   6 )")
    ("" "      let f = proc(x:?) 10
      in letrec ? f(x:?) = if zero?(x) then 1 else ( f   ( x - 1 ) )
         in ( f   3 )")
    ("" "      let f = proc(x:?) ( x - 11 )
      in ( f   zero? (7) )")
    ("" "      let twice = proc(f:?) proc(x:?) ( f   ( f   x ) )
      in twice")
    ("" "      let twice = proc(f:?) proc(x:?) ( f   ( f   x ) )
      in ( twice   proc(x:?) x )")
    ("" "      proc(x:?) zero?(x)")
    ("" "      proc(f:?) proc(x:?) ( f   x )")
    ("" "      letrec ? f(x:?) = ( f   x )
      in f")
    ("" "      letrec ? f(x:?) = ( f   x )
      in ( f   3 )")
    ("" "      letrec ? f(x:?) = ( f   x )
      in zero?( ( f   3 ))")
    ("" "      let compose = proc(f:?) proc(g:?) proc(x:?) ( f   ( g   x ) )
      in compose")
    ("" "      let compose = proc(f:?) proc(g:?) proc(x:?) ( f   ( g   x ) )
      in ( compose   proc(x:?) ( x - 1 ) )")
    ("" "      let compose = proc(f:?) proc(g:?) proc(x:?) ( f   ( g   x ) )
      in ( ( compose   proc(x:?) ( x - 1 ) )   proc(x:?) ( x - 1 ) )")
    ("" "      proc(c:?) proc(b:?) proc(a:?) ( ( c   b )   ( b   a ) )")
    ("" "      let pair = proc(x:?) proc(f:?) ( ( f   x )   x )
      in pair")
    ("" "      let id = proc(x:?) x
      in if zero? (0) then 1 else ( id   1 )")
    ("" "      let id = proc(x:?) x
      in if ( id   zero?(0) ) then 1 else ( id   1 )")
    ("" "      let id = proc(x:?) x
      in if ( proc(x:?) x   zero?(0) ) then 1 else ( proc(x:?) x   1 )")
    ))
