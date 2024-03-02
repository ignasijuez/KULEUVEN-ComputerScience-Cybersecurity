#lang racket
(provide (all-defined-out))

(define examples-with-notes
  '( 
    ("object representation" "
class c1 extends object
  field i
  field j
  method initialize ( )
    begin
      set i = 333;
      set j = 444;
    end
let
  o1 = new c1 ( )
in o1")
    ("method environments" "
class c1 extends object
  field i
  field j
  method initialize ( )
    begin
      set i = 333;
      set j = 444;
    end
  method sub(d)
    begin
      set i = (i - d);
      set j = (j - d);
    end
let
  o1 = new c1 ( )
in send o1 sub(11)")
    ("method environments with subclasses" "
class c1 extends object
  field i
  field j
  method initialize ( )
    begin
      set i = 333;
      set j = 444;
    end
  method sub(d)
    begin
      set i = (i - d);
      set j = (j - d);
    end
class c2 extends c1
  field i
  field k
  method sub(d) super sub(d)
let
  o1 = new c2 ( )
in send o1 sub(11)")
    ("encapsulation for state invariants" "
class c1 extends object
  field i
  field j
  method initialize ( x )
    begin
      set i = x;
      set j = ( 0 - x );
    end
  method countup ( d )
    begin
      set i = ( i + d );
      set j = ( j - d );
    end
  method getstate ( )
    [ i, j ]
let
  t1 = 0
  t2 = 0
  o1 = new c1 ( 3 )
in begin
     set t1 = send o1 getstate ( );
     send o1 countup ( 2 );
     set t2 = send o1 getstate ( );
     [ t1, t2 ];
   end")
    ("Dynamic dispatch" "
class interior_node extends object
  field left
  field right
  method initialize ( l, r )
    begin
      set left = l;
      set right = r;
    end
  method sum ( )
    ( send left sum ( ) + send right sum ( ) )
class leaf_node extends object
  field value
  method initialize ( v )
    set value = v
  method sum ( )
    value
let
  o1 = new interior_node ( new interior_node ( new leaf_node ( 3 ), new leaf_node ( 4 ) ), new leaf_node ( 5 ) )
in send o1 sum ( )")
    ("Recursion on self" "
class oddeven extends object
  method initialize ( )
    1
  method even ( n )
    if zero?(n) then 1 else send self odd ( ( n - 1 ) )
  method odd ( n )
    if zero?(n) then 0 else send self even ( ( n - 1 ) )
let
  o1 = new oddeven ( )
in send o1 odd ( 13 )")
    ("Inheritance and subclass polymorphism" "
class point extends object
  field x
  field y
  method initialize ( initx, inity )
    begin
      set x = initx;
      set y = inity;
    end
  method move ( dx, dy )
    begin
      set x = ( x + dx );
      set y = ( y + dy );
    end
  method get_location ( )
    [ x, y ]
class colorpoint extends point
  field color
  method set_color ( c )
    set color = c
  method get_color ( )
    color
let
  p = new point ( 3, 4 )
  cp = new colorpoint ( 10, 20 )
in begin
     send p move ( 3, 4 );
     send cp set_color ( 87 );
     send cp move ( 10, 20 );
     [ send p get_location ( ), send cp get_location ( ), send cp get_color ( ) ];
   end")
    ("Uninitialized field" "
class point extends object
  field x
  field y
  method initialize ( initx, inity )
    begin
      set x = initx;
      set y = inity;
    end
  method move ( dx, dy )
    begin
      set x = ( x + dx );
      set y = ( y + dy );
    end
  method get_location ( )
    [ x, y ]
class colorpoint extends point
  field color
  method set_color ( c )
    set color = c
  method get_color ( )
    color
let
  p = new point ( 3, 4 )
  cp = new colorpoint ( 10, 20 )
in begin
     send p move ( 3, 4 );
     send cp move ( 10, 20 );
     [ send p get_location ( ), send cp get_location ( ), send cp get_color ( ) ];
   end")
    ("Field shadowing" "
class c1 extends object
  field x
  field y
  method initialize ( )
    1
  method setx1 ( v )
    set x = v
  method sety1 ( v )
    set y = v
  method getx1 ( )
    x
  method gety1 ( )
    y
class c2 extends c1
  field y
  method sety2 ( v )
    set y = v
  method getx2 ( )
    x
  method gety2 ( )
    y
let
  o2 = new c2 ( )
in begin
     send o2 setx1 ( 101 );
     send o2 sety1 ( 102 );
     send o2 sety2 ( 999 );
     [ send o2 getx1 ( ), send o2 gety1 ( ), send o2 getx2 ( ), send o2 gety2 ( ) ];
   end")
    ("Method overriding" "
class c1 extends object
  method initialize ( )
    1
  method m1 ( )
    11
  method m2 ( )
    send self m1 ( )
class c2 extends c1
  method m1 ( )
    22
let
  o1 = new c1 ( )
  o2 = new c2 ( )
in [ send o1 m1 ( ), send o2 m1 ( ), send o2 m2 ( ) ]")
    
    ("super calls" "
class c1 extends object
  method initialize ( )
    1
  method m1 ( )
    send self m2 ( )
  method m2 ( )
    13
class c2 extends c1
  method m1 ( )
    22
  method m2 ( )
    23
  method m3 ( )
    super m1 ( )
class c3 extends c2
  method m1 ( )
    32
  method m2 ( )
    33
let
  o3 = new c3 ( )
in send o3 m3 ( )")

    ("open recursion" "
class D extends object
  method initialize ( )
    1
  method double ( n )
    if zero?(n) then 0 else (send self double ( ( n - 1 ) )  - -2)
let
  o1 = new D ( )
in send o1 double ( 13 )

")

    ))