-- class Foo { 
    
--     bar 
--     };

class Main {
    data_member : Int <- 23;
    o : Object;
    a : Int;
    main() : Object { 
        {
        isvoid a;
        let x : Int in a;
        let a : Int <- 4, b : Int <- 5, c : Int <- 6 in  a;
        let x1 : Int in
            let x2 : Int in
                let x3 : Object in
                    x1 < x2;
        }
    };
    method(init : Int, des : Object): Int {
        {
        a <- 5;
        x1 <- 1;
        x2 <- 2;
        if isvoid 4 then 5 else 6 fi;
        while 0 loop 1 pool;
        b <- new Int;
        a <- x1 + x2;
        a <- x2 - x2;
        a <- x1 * x2;
        a <- x2 / x1;
        a <- ~x1;
        a <- (x1 < x2);
        a <- (x1 < x2); 
        a <- (x1 <= x2);
        a <- (not (x1 < x2));
        a <- x1;
        str <- "stareg";
        b <- true;
        b <- false;

        case a of
            id1 : Int => 3;
            id2 : Object => new Object;
        esac;
        }
    }; 

};

class A {
    ana(): Int {
        x <- 3
    };
    ana2(): Int {
        (new Foo1).func()
    };
};

Class BB__ inherits A {
    ana(): Int {
        self@A.ana()
    };
};
