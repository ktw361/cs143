class Main {
my_a : A <- new A;
vd : Object;
ia : Int <- 12;
  main() : Object {
{
  case my_a of
    o1 : Bool => (new IO).out_int(1);
    -- o1 : Int => (new IO).out_int(2);
    -- o2 : A => (new IO).out_int(o2.f());
    --o2 : B => (new IO).out_int(o2.f());
    --o2 : C => (new IO).out_int(o2.h());
    --default : Object => (new IO).out_string("default");
  esac;
}
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
