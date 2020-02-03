
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main {
  data : Int <- 203; -- attr, init
  my_a : A <- new A; -- new in attr init
  my_str : String;
  main():Int { 
{
  (new IO).out_int(data).out_string("\n"); -- 203
  data <- 200;        -- attr-var, assign
  data <- data + 4;   -- plus, int const
  (new IO).out_int(data).out_string("\n"); -- new, dispatch
  my_str <- "sss";    -- string const
  (new IO).out_string(my_str).out_string("\n");
  true;               -- true const
  false;              -- false const
  my_a <- new B; -- assign subclass
  (new IO).out_int(my_a@A.f()).out_string("\n"); -- statis dispatch, should produe 1
  (new IO).out_int(my_a.f()).out_string("\n");   -- dispatch, should produe 2

  if data = 204 then
    (new IO).out_string("eval true\n")
  else
    (new IO).out_string("bad\n")
  fi;           -- if-true, equal
    
  if data = 2005 then
    (new IO).out_string("bad\n")
  else
    (new IO).out_string("eval false\n")
  fi;           -- if-false

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
