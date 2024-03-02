#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '(
    ("" "
letrec
  noisy(l) = if null?(l)
             then 0
             else begin
                    print(car(l));
                    ( noisy   cdr(l) );
                  end
in begin
     spawn( proc(d) ( noisy   [ 1, 2, 3, 4, 5 ] ) );
     spawn( proc(d) ( noisy   [ 6, 7, 8, 9, 10 ] ) );
     print(100);
   end
")
    ("" "
letrec
  noisy(l) = if null?(l)
             then 0
             else begin
                    print(car(l));
                    yield();
                    ( noisy   cdr(l) );
                  end
in begin
     spawn( proc(d) ( noisy   [ 1, 2, 3, 4, 5 ] ) );
     spawn( proc(d) ( noisy   [ 6, 7, 8, 9, 10 ] ) );
     print(100);
   end")
    ("" "
let buffer = 0
in let producer = proc(n) letrec
                            wacht (k) = if zero?(k)
                                      then set buffer = n
                                      else begin
                                             print(( k - -200 ));
                                             ( wacht   ( k - 1 ) );
                                           end
                          in ( wacht   5 )
   in let consumer = proc(d) letrec
                               busywait(k) = if zero?(buffer)
                                             then begin
                                                    print(( k - -100 ));
                                                    ( busywait   ( k - -1 ) );
                                                  end
                                             else buffer
                             in ( busywait   0 )
      in begin
           spawn( proc(d) ( producer   44 ) );
           print(300);
           ( consumer   86 );
         end")
    ("" "
let x = 0
in let incr_x = proc(id) proc(dummy) begin
                                       set x = ( x - -1 );
                                       print(x);
                                     end
   in begin
        spawn( ( incr_x   100 ) );
        spawn( ( incr_x   200 ) );
        spawn( ( incr_x   300 ) );
      end")
    ("" "
let x = 0
in let incr_x = proc(id) proc(dummy) begin
                                       set x = ( x - yield() );
                                       print(x);
                                     end
   in begin
        spawn( ( incr_x   100 ) );
        spawn( ( incr_x   200 ) );
        spawn( ( incr_x   300 ) );
      end")
    ("" "
let x = 0
in let mut = mutex()
   in let incr_x = proc(id) proc(dummy) begin
                                          wait( mut );
                                          set x = ( x - -1 );
                                          signal( mut );
                                          print(x);
                                        end
      in begin
           spawn( ( incr_x   100 ) );
           spawn( ( incr_x   200 ) );
           spawn( ( incr_x   300 ) );
         end")
    ("" "
let x = 0
in let mut = mutex()
   in let incr_x = proc(id) proc(dummy) begin
                                          wait( mut );
                                          set x = ( x - yield() );
                                          signal( mut );
                                          print(x);
                                        end
      in begin
           spawn( ( incr_x   100 ) );
           spawn( ( incr_x   200 ) );
           spawn( ( incr_x   300 ) );
         end")
    ("" "
let m = mutex()
in begin
     wait( m );
     spawn( proc(dummy) begin
                          wait( m );
                          print(300);
                        end );
     200;
   end")
  ))