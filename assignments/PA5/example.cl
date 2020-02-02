
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main {
  main():Int { 0 };
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
