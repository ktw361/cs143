-- class F1 {};
-- class F2 inherits F1 {};
-- class FF inherits F2 {};
-- class A  inherits F1 {};

class C {
	a : Int;
	self : Int;
	b : Bool;
    t1 : F2;
    run(self : Int) : Object {
        new Object
    };
	init(x : Int, x : Bool) : Int {
        {
        run(a);
        a1;
        a <- b;
        if a then a else a fi; -- none-Bool in predicate
            -- if (new Int) then a else a fi; -- none-Bool in predicate
        -- t1 <- new F1;
        -- t1 <- new A;
        let x : Int in 5;
        case a of
        self : Int => a1;
        a2 : Int => a2;
        esac;
        while a
            loop a
            pool;
        not a; -- comp
        ~b; -- neg
        b < b;
        b <= b;
        -- case a of 
        --     a1 : Int => 5;
        -- esac;
		a <- x;
        new F1 = new F2;
        new Int = new Bool;
        a + b;
        a * b;
        a - b;
        a / b;
		-- b <- y;
		new Object;
        }
	};
};

class C1 inherits C {
    a : Bool;
    init(x:Int , y:Bool) :Int {
        new Int
    };
};

-- Class Main {
-- 	main():C {
-- 	 {
-- 	  (new A)@C.init(1,true);
-- 	  (new C1)@C.init2(1,true);
-- 	  (new C1)@C1.init(1,true);
-- 	  (new C).init(1,1);
-- 	  (new C).init(1,true,3);
-- 	  (new C).iinit(1,true);
-- 	  (new C);
-- 	 }
-- 	};
-- };
