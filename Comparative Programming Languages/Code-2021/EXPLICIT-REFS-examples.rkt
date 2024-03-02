#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '(
    ("" "
let x = newref ( 100 )
in begin
     setref x = 110;
     x;
   end")
    ("" "
let x = newref ( 100 )
in begin
     setref x = 110;
     deref ( x );
   end")
    ("" "
let x = newref ( 100 )
in begin
     setref x = 110;
     setref x = ( deref ( x ) - 1 );
     deref ( x );
   end")
    ("" "
 let x = newref ( 0 )
in letrec
     even(dummy) = if zero?(deref ( x ))
                   then 1
                   else begin
                          setref x = ( deref ( x ) - 1 );
                          ( odd   888 );
                        end
                        
     odd(dummy) = if zero?(deref ( x ))
                  then 0
                  else begin
                         setref x = ( deref ( x ) - 1 );
                         ( even   888 );
                       end
                       
   in begin
        setref x = 13;
        ( odd   888 );
      end")
    ("" "
let g = let counter = newref ( 0 )
        in proc(dummy) begin
                         setref counter = ( deref ( counter ) - -1 );
                         deref ( counter );
                       end
                       
in let a = ( g   11 )
   in let b = ( g   11 )
      in ( a - b )")
    ("Higher-order store: store can contain proc-vals" "
newref ( proc(dummy) 10 )")
    ("Cyclic data structures" "
let l1 = newref ( 0 )
in let l2 = newref ( l1 )
   in setref l1 = l2")
    ("Aliasing" "
let l1 = newref ( 0 )
in let l2 = l1
   in begin
        setref l1 = 10;
        deref ( l2 );
      end")
    ("" "
newref ( newref ( newref ( 0 ) ) )")
    ("Long cyclic chain of pointers" "
let b = newref ( 0 )
in let e = newref ( b )
   in letrec
        chain(n) = if zero?(n)
                   then 0
                   else let x = newref ( 0 )
                        in begin
                             setref deref ( e ) = x;
                             setref e = x;
                             ( chain   ( n - 1 ) );
                           end         
      in begin
           ( chain   10 );
           setref deref ( e ) = b;
         end")
    ("Recursive definitions using the store" "
let double = newref ( 0 )
in begin
     setref double = proc(n) if zero?(n)
                             then 0
                             else ( 2 - ( 0 - ( deref ( double )   ( n - 1 ) ) ) );
     ( deref ( double )   10 );
   end")
   
  ))