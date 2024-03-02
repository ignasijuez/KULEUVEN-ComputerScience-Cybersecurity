#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '(
    ("" "
let x = 1
in begin
     set x = 2;
     x;
   end
")
    ("side-effects" "
let p = let y = 1
        in proc(dummy) begin
                         set y = ( y - 1 );
                         y;
                       end
                       
in begin
     ( p   0 );
     ( p   0 );
     ( p   0 );
     ( p   0 );
   end
")
    ("no referential transparency" "
let g = let counter = 0
        in proc(dummy) begin
                         set counter = ( counter - -1 );
                         counter;
                       end
                       
in ( ( g   11 ) - ( g   11 ) )")
    ("stupid handling of recursive procedure lookup" "
letrec
  f(x) = 1
in begin
     f;
     f;
     f;
   end")
    ("stupid handling of recursive procedure lookup" "
let x = 0
in letrec
     even(dummy) = if zero?(x)
                   then 1
                   else begin
                          set x = ( x - 1 );
                          ( odd   888 );
                        end
                        
     odd(dummy) = if zero?(x)
                  then 0
                  else begin
                         set x = ( x - 1 );
                         ( even   888 );
                       end
                       
   in begin
        set x = 13;
        ( odd   888 );
      end")
    ("simulating letrec" "
let
  double = 0
in begin
     set double = proc(x) if zero?(x) then 0 else ( (double ( x - 1)) - -2);
     (double 10);
   end")
    ))