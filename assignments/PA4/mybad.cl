class F1 {};
class F2 inherits F1 {};
class FF inherits F2 {};
class A  inherits F1 {};

class C {
	a : Intt <- new As;
	b : Bool;
    t1 : F2;
	init(x : Int, y : Bool) : Objectt{
        {
        t1 <- new F1;
        t1 <- new A;
        let x : Int in 5;
        case a of 
            a1 : Int => 5;
        esac;
		a2 <- x;
		b <- y;
		self;
        }
	};
};

Class Main {
	main():C {
	 {
	  (new C).init(1,1);
	  (new C).init(1,true,3);
	  (new C).iinit(1,true);
	  (new C);
	 }
	};
};
