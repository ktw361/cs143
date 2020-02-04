
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main {
  data : Int <- 203; -- attr, init
  my_a : A <- new A; -- new in attr init
  my_str : String;
  my_void : A;
  my_io : MyIO <- new MyIO;

  main():Int { 
{
  (new IO).out_int(data).out_string("\n"); -- 203
  data <- 200;        -- attr-var, assign
  data <- data + 4;   -- plus, int const
  my_io.out_int(data).out_string("\n"); -- new, dispatch
  my_str <- "test string const";    -- string const
  my_io.out_string(my_str).out_string("\n");
  true;               -- true const
  false;              -- false const
  my_a <- new B; -- assign subclass
  my_io.out_int(my_a@A.f()).out_string("\n"); -- statis dispatch, should produe 1
  my_io.out_int(my_a.f()).out_string("\n");   -- dispatch, should produe 2

  my_io.outln(3*4); -- mul
  my_io.outln(3-4); -- sub
  my_io.outln(18 / 6); -- div

  if data = 204 then my_io.out_string("eval true\n")
  else my_io.out_string("bad\n") fi;           -- if-true, equal
  if data = 2005 then my_io.out_string("bad\n")
  else my_io.out_string("eval false\n") fi;           -- if-false

  if data < 2005 then my_io.out_string("less\n")
  else my_io.out_string("bad\n") fi;           -- less
  if data <= 204 then my_io.out_string("less-equal\n")
  else my_io.out_string("bad\n")fi;           -- leq
  if  0 - data = ~204 then my_io.out_string("neg\n")
  else my_io.out_string("bad\n")fi;           -- neg
  if not data <= 205 then my_io.out_string("bad\n")
  else my_io.out_string("not\n")fi;           -- not
  
  if isvoid my_void then my_io.out_string("isvoid true\n")
  else my_io.out_string("bad\n")fi;           -- isvoid-true
  if isvoid my_io then my_io.out_string("bad\n")
  else my_io.out_string("isvoid false\n")fi;  -- isvoid-false

  let a:Int<-33,a:Int<-44in my_io.outln(a); -- 44, let-init
  let local : Int in my_io.outln(data + local); -- 204, let-no-init

  while (data <- data + 1) < 207 loop
  {
    my_io.outln(data);
  }
  pool; -- 205, 206

  my_void.f();  -- dispatch on void // TODO
  my_io.out_string("end of main()\n");
  data;         -- to conform return type
} -- sequence
};
};

class A {
  a : Int;
  f() : Int { 1};
};

class B inherits A {
  b : Int;
  f() : Int { 2};
};

class C inherits A {
  c : Int;
  h() : Int { 3+ 44};
};

Class Foo inherits C {
};

Class MyIO inherits IO {
  myio : IO <- new IO;
  outln(a : Int) : IO {
    myio.out_int(a).out_string("\n")
};
};

