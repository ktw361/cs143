
(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

class A {
    func() : Int {
        {
        let Int : Int <- 5, a : Int <-5  in x;
        }
    };
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

class A {
    data_member : Int;
    data2 : 
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* error:  closing brace is missing *)
Class E inherits A {
;

