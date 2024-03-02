#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '(
    ("" "
let p = newpair ( 22 , 33 )
in left(p)")
    ("" "
let p = newpair ( 22 , 33 )
in right(p)")
    ("" "
let p = newpair ( 22 , 33 )
in begin
     setleft   p   =   77;
     left(p);
   end")
   ("" "
let p = newpair ( 22 , 33 )
in begin
     setleft   p   =   77;
     right(p);
   end")
   ("" "
let p = newpair ( 22 , 33 )
in begin
     setright   p   =   77;
     right(p);
   end")
   ("" "
let p = newpair ( 22 , 33 )
in begin
     setright   p   =   77;
     left(p);
   end")
   ("" "
let g = let count = newpair ( 0 , 0 )
        in proc(dummy) begin
                         setleft   count   =   ( left(count) - -1 );
                         left(count);
                       end
in ( ( g   22 ) - ( g   22 ) )")
   ("" "
let g = let count = newpair ( 0 , 0 )
        in proc(dummy) begin
                         setright   count   =   ( right(count) - -1 );
                         right(count);
                       end
in ( ( g   22 ) - ( g   22 ) )")
   ("" "
let glo = newpair ( 11 , 22 )
in let f = proc(loc) begin
                       setright   loc   =   left(loc);
                       setleft   glo   =   99;
                       ( left(loc) - right(loc) );
                     end
   in ( f   glo )")
   ("" "
letrec
  list(n) = if zero?(n) then 0 else newpair ( n , ( list   ( n - 1 ) ) )
in ( list   10 )")
   ("recursive list through assignment" "
let list = 0
in begin
     set list = proc(n)
                    if zero? ( n )
                    then 0
                    else newpair ( n , ( list ( n - 1 ) ) );
     ( list 10 );
   end")
  ))