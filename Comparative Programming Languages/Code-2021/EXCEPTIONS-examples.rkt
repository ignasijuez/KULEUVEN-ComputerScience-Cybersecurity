#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '(
    ("lists" "
[ 1, 2, 3 ]")
    ("" "
car([ 1, 2, 3 ])")
    ("" "
cdr([ 1, 2, 3 ])")
    ("" "
let safecdr = proc(l) if null?(l)
                      then raise 0
                      else cdr(l)
in try ( safecdr   [ ] )
   catch(x)
   x")
    ("" "
let safecdr = proc(l) if null?(l)
                      then raise 0
                      else cdr(l)
in try ( safecdr   [ 1, 2 ] )
   catch(x)
   x")
    ("" "
let safecdr = proc(l) if null?(l)
                      then raise 0
                      else cdr(l)
in try ( ( safecdr   [ ] ) - 1 )
   catch(x)
   x")
    ("deep uncaught exception" "
letrec double(x) = if zero?(x)
                   then raise 0
                   else ( ( double   ( x - 1 ) ) - -2 )
in ( double   6 )")
    ("deep caught exception" "
letrec double(x) = if zero?(x)
                   then raise 0
                   else ( ( double   ( x - 1 ) ) - -2 )
in try ( double   6 )
   catch(x)
   x")
  ))