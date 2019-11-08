(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class List {
   -- Define operations on empty lists.

   isNil() : Bool { true };

   head()  : String { { abort(); ""; } };

   tail()  : List { { abort(); self; } };

   cons(i : String) : List {
      (new Cons).init(i, self)
   };

};

class Cons inherits List {

   car : String;	-- The element in this list cell

   cdr : List;	-- The rest of the list

   isNil() : Bool { false };

   head()  : String { car };

   tail()  : List { cdr };

   init(i : String, rest : List) : List {
      {
	 car <- i;
	 cdr <- rest;
	 self;
      }
   };

};

class Main inherits IO {

    stack : List;
    inchar : String;
    str_tmp1 : String;
    str_tmp2 : String;
    int_tmp1 : Int;
    int_tmp2 : Int;

    display_stacklist(l : List) : Object {
        if l.isNil() 
            then 0
        else {
            out_string(l.head());
            out_string("\n");
            display_stacklist(l.tail());
        }
        fi
    };

    eval_list(l : List) : List {
        if not l.isNil() then {
            if l.head() = "+" then {
                l <- l.tail();
                int_tmp1 <- (new A2I).a2i(l.head());
                l <- l.tail();
                int_tmp2 <- (new A2I).a2i(l.head());
                l <- l.tail();
                l.cons((new A2I).i2a(int_tmp1 + int_tmp2));  -- return expression
            } else 
            if l.head() = "s" then {
                l <- l.tail();
                str_tmp1 <- l.head();
                l <- l.tail();
                str_tmp2 <- l.head();
                l.tail().cons(str_tmp1).cons(str_tmp2); -- return expression
            } else l
            fi fi;
        } else l
        fi
    };

    main() : Object {
        {
            stack <- new List;
            inchar <- "";
            while not inchar = "x" loop {
                out_string(">");
                inchar <- (new IO).in_string();
                if inchar = "+" then stack <- stack.cons(inchar) else
                if inchar = "s" then stack <- stack.cons(inchar) else
                if inchar = "x" then 0 else
                if inchar = "d" then display_stacklist(stack) else
                if inchar = "e" then stack <- eval_list(stack) else
                    stack <- stack.cons(inchar) -- push int
                fi fi fi fi fi;
            }
            pool;
        }
    };

};
