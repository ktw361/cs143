class F1 {};
class F2 inherits F1 {};
class FF inherits F2 {};
class A  inherits F1 {};

class C {
	a : Int;
	b : Bool;
    t1 : F2;
	init(x : Int, y : Bool) : Int {
        {
            a1;
            a <- b;
        -- t1 <- new F1;
        -- t1 <- new A;
        let x : Int in 5;
        -- case a of 
        --     a1 : Int => 5;
        -- esac;
		a <- x;
		-- b <- y;
		new Object;
        }
	};
};

class C1 inherits C {};

Class Main {
	main():C {
	 {
	  (new A)@C.init(1,true);
	  (new C1)@C.init2(1,true);
	  (new C1)@C1.init(1,true);
	  (new C).init(1,1);
	  (new C).init(1,true,3);
	  (new C).iinit(1,true);
	  (new C);
	 }
	};
};
