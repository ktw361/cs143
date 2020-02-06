

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <map>
#include <deque>
// for debug
#include <typeinfo>


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}


/**** Implementation of added function in cool-tree.h ****/
/* class__class */
typedef List<Class__class> IGnode_list;
Symbol class__class::get_name() { return name; }
Symbol class__class::get_parent() { return parent; }
Features class__class::get_features() { return features; }
IGnode_list *class__class::get_children() {
    return children;
}
void class__class::add_children(Class_ c) { 
    children = new IGnode_list(c, children); 
}

/* Feature */
Formals Feature_class::get_formals() { return nil_Formals(); }

/* method_class */
bool method_class::is_method() { return true; }
Symbol method_class::get_name() { return name; }
Formals method_class::get_formals() { return formals; }
Symbol method_class::get_type() { return return_type; }
Expression method_class::get_expr() { return expr; }

/* attr_class */
bool attr_class::is_method() { return false; }
Symbol attr_class::get_name() { return name; }
Symbol attr_class::get_type() { return type_decl; }
Expression attr_class::get_expr() { return init; }

/* formal_class */
Symbol formal_class::get_name() { return name; }
Symbol formal_class::get_type() { return type_decl; }

// The following functions are not virtual, 
// and Base class Expression defines its own get_type();
//
/* let_class */
Symbol let_class::get_iden() { return identifier; }
Symbol let_class::get_type() { return type_decl; }
Expression let_class::get_init() { return init; }
Expression let_class::get_body() { return body; }

/* typcase_class */
Expression typcase_class::get_expr() { return expr; }
Cases typcase_class::get_cases() { return cases; }

/* branch_class */
Symbol branch_class::get_name() { return name; }
Symbol branch_class::get_type() { return type_decl; }
Expression branch_class::get_expr() { return expr; }

/* block_class */
Expressions block_class::get_body() { return body; }

/* assign_class */
Symbol assign_class::get_name() { return name; }
Expression assign_class::get_expr() { return expr; }

/* new__class */
Symbol new__class::get_type() { return type_name; }

/* dispatch_class */
Expression dispatch_class::get_expr() { return expr; }
Symbol dispatch_class::get_name() { return name; }
Expressions dispatch_class::get_actual() { return actual; }

/* static_dispatch_class */
Expression static_dispatch_class::get_expr() { return expr; }
Symbol static_dispatch_class::get_type() { return type_name; }
Symbol static_dispatch_class::get_name() { return name; }
Expressions static_dispatch_class::get_actual() { return actual; }

/* object_class */
Symbol object_class::get_name() { return name; }

/* cond_class */
Expression cond_class::get_pred() { return pred; }
Expression cond_class::get_then() { return then_exp; }
Expression cond_class::get_else() { return else_exp; }

/* loop_class */
Expression loop_class::get_pred() { return pred; }
Expression loop_class::get_body() { return body; }

/* isvoid_class */
Expression isvoid_class::get_expr() { return e1; }

/* comp_class */
Expression comp_class::get_expr() { return e1; }

/* lt_class */
Expression lt_class::get_lhs() { return e1; }
Expression lt_class::get_rhs() { return e2; }

/* leq_class */
Expression leq_class::get_lhs() { return e1; }
Expression leq_class::get_rhs() { return e2; }

/* eq_class */
Expression eq_class::get_lhs() { return e1; }
Expression eq_class::get_rhs() { return e2; }

/* neg_class */
Expression neg_class::get_expr() { return e1; }

/* plus_class */
Expression plus_class::get_e1() { return e1; }
Expression plus_class::get_e2() { return e2; }

/* sub_class */
Expression sub_class::get_e1() { return e1; }
Expression sub_class::get_e2() { return e2; }

/* divide_class */
Expression divide_class::get_e1() { return e1; }
Expression divide_class::get_e2() { return e2; }

/* mul_class */
Expression mul_class::get_e1() { return e1; }
Expression mul_class::get_e2() { return e2; }


/**** end of implementation in cool-tree.h ****/

// Can't use plain function, as tree_node doesn't have get_name()
#define dump_fname_lineno(s, n)             \
(s) << (n)->get_filename() << ":" <<  (n)->get_line_number() << ": "

/*** Type checker ***/
Symbol ClassTable::typecheck_expr(Expression expr) {
    Symbol ret = Object;
    if (dynamic_cast<object_class*>(expr)) {
        ret = typecheck_var(expr);
    } else if (dynamic_cast<assign_class*>(expr)) {
		ret = typecheck_assign(expr);
	} else if (dynamic_cast<bool_const_class*>(expr)) {
		ret = typecheck_bool(expr);
	} else if (dynamic_cast<int_const_class*>(expr)) {
		ret = typecheck_int(expr);
	} else if (dynamic_cast<string_const_class*>(expr)) {
		ret = typecheck_string(expr);
	} else if (dynamic_cast<new__class*>(expr)) {
		ret = typecheck_new(expr);
	} else if (dynamic_cast<dispatch_class*>(expr)) {
		ret = typecheck_dispatch(expr);
	} else if (dynamic_cast<static_dispatch_class*>(expr)) {
		ret = typecheck_static_dispatch(expr);
	} else if (dynamic_cast<cond_class*>(expr)) {
		ret = typecheck_cond(expr);
	} else if (dynamic_cast<block_class*>(expr)) {
		ret = typecheck_block(expr);
	} else if (dynamic_cast<let_class*>(expr)) {
		ret = typecheck_let(expr);
	} else if (dynamic_cast<typcase_class*>(expr)) {
		ret = typecheck_case(expr);
	} else if (dynamic_cast<loop_class*>(expr)) {
		ret = typecheck_loop(expr);
	} else if (dynamic_cast<isvoid_class*>(expr)) {
		ret = typecheck_isvoid(expr);
	} else if (dynamic_cast<neg_class*>(expr)) {
		ret = typecheck_neg(expr);
	} else if (dynamic_cast<lt_class*>(expr)) {
		ret = typecheck_compare(expr);
	} else if (dynamic_cast<leq_class*>(expr)) {
		ret = typecheck_compare(expr);
	} else if (dynamic_cast<comp_class*>(expr)) {
		ret = typecheck_comp(expr);
	} else if (dynamic_cast<plus_class*>(expr)) {
		ret = typecheck_arith(expr);
	} else if (dynamic_cast<sub_class*>(expr)) {
		ret = typecheck_arith(expr);
	} else if (dynamic_cast<mul_class*>(expr)) {
		ret = typecheck_arith(expr);
	} else if (dynamic_cast<divide_class*>(expr)) {
		ret = typecheck_arith(expr);
	} else if (dynamic_cast<eq_class*>(expr)) {
		ret = typecheck_equal(expr);
	} else {
        semant_error();
        dump_fname_lineno(cerr, cls_env) 
            << "Fatal Error: Expression type not found.";
        return Object;
    }
    return ret;
}

Symbol ClassTable::typecheck_var(Expression expr) {
    object_class *e = dynamic_cast<object_class*>(expr);
    Symbol s = e->get_name();
    Symbol *pT = obj_env.lookup(s);
    if (pT == NULL) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Undeclared identifier " << s << "." << endl;
        return Object;
    }
    expr->set_type(*pT);
    return *pT;
}

Symbol ClassTable::typecheck_assign(Expression expr) {
    assign_class *e = dynamic_cast<assign_class*>(expr);
    Symbol id = e->get_name();
    // Test self assignment
    if (id == self) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Cannot assign to 'self'." << endl;
        return Object;
    }
    Symbol T, *pT = obj_env.lookup(id);
    // Assign to undeclared?
    if (pT == NULL) {
        dump_fname_lineno(cerr, cls_env)
            << "Assignment to undeclared variable "
            << id << "." << endl;
        return Object;
    }
    T = *pT;

    Expression e1 = e->get_expr();
    Symbol T1 = typecheck_expr(e1);
    if (!conform(T1, T)) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Type " << T1 << " of assigned expression does not"
            << " conform to declared type " << T 
            << " of identifier " << id << "." << endl;
        return Object;
    }
    expr->set_type(T1);
    return T1;
}

Symbol ClassTable::typecheck_bool(Expression expr) {
    expr->set_type(Bool);
    return Bool;
}

Symbol ClassTable::typecheck_int(Expression expr) {
    expr->set_type(Int);
    return Int;
}

Symbol ClassTable::typecheck_string(Expression expr) {
    expr->set_type(Str);
    return Str;
}

Symbol ClassTable::typecheck_new(Expression expr) {
    new__class *e = dynamic_cast<new__class*>(expr);
    Symbol T = e->get_type();
    if (T != SELF_TYPE && !ig_nodes.count(T)) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "'new' used with undefined class "
            << T << ".\n";
        return Object;
    }
    expr->set_type(T);
    return expr->get_type();
}

Symbol ClassTable::typecheck_dispatch(Expression expr) {
    dispatch_class *e = dynamic_cast<dispatch_class*>(expr); 
    Expression e0 = e->get_expr();
    Symbol name = e->get_name();  // name f
    Expressions actual = e->get_actual();
    Symbol T0 = typecheck_expr(e0);
    Symbol T_search = (T0 == SELF_TYPE) ? cls_env->get_name() : T0;
    if (semant_debug)  
        cout << "[INFO] e0 type: " << T0 << " -> " << T_search << endl;

    MtdKeyType key = std::make_pair(T_search, name);
    while(!method_env.count(key)) {
        if (semant_debug) 
            cout <<  "[INFO] Method " << name << " not found in class "
                << T_search << ", searching parent..." << endl;
        if (T_search == Object) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Dispatch to undefined method " << name << "." << endl;
            return Object;
        }
        T_search = ig_nodes[T_search]->get_parent();
        key = std::make_pair(T_search, name);
    }
    MtdValType signatures = *method_env[key];
    int num_formals_decl = *reinterpret_cast<int*>(signatures[NUM_FORMALS]);
    if (num_formals_decl != actual->len()) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Method " << name << " called with wrong number of arguments.\n";
        return Object;
    }
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol Ti = typecheck_expr(actual->nth(i));
        Symbol Ti_decl = signatures[i];
        if (!conform(Ti, Ti_decl)) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "In call of method " << name << ", type " << Ti
                << " does not conform to declared type " << Ti_decl << ".\n";
            return Object;
        }
    }
    Symbol T_ret = signatures[actual->len()];
    if (semant_debug) 
        cout << "[INFO] Method " << name << " return type: " << T_ret << " -> ";
    T_ret = (T_ret == SELF_TYPE) ? T0 : T_ret;
    if (semant_debug)
        cout << T_ret << endl;
    expr->set_type(T_ret);
    return T_ret;
}

Symbol ClassTable::typecheck_static_dispatch(Expression expr) {
    // e0@T.name(e1, e2, ..., en)
    static_dispatch_class *e = dynamic_cast<static_dispatch_class*>(expr);
    Expression e0 = e->get_expr();
    Symbol T = e->get_type();       // target typename
    if (T == SELF_TYPE) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Static dispatch to SELF_TYPE." << endl; 
        return Object;
    }
    Symbol name = e->get_name();    // name f
    Expressions actual = e->get_actual();
    Symbol T0 = typecheck_expr(e0);
    if (semant_debug)  cout << "[INFO] e0 type: " << T0 << endl;
    if (!conform(T0, T)) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Expression type " << T0 
            << " does not conform to declared static dispatch type "
            << T << "." << endl;
        return Object;
    }
    MtdKeyType key = std::make_pair(T, name);
    Symbol T_search = T0;
    while(!method_env.count(key)) {
        if (semant_debug) 
            cout <<  "[INFO] Method: " << name << " not found in class "
                << T_search << ", searching parent..." << endl;
        T_search = ig_nodes[T_search]->get_parent();
        key = std::make_pair(T_search, name);
        if (T_search == Object) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Static dispatch to undefined method " << name << "." << endl;
            return Object;
        }
    }
    MtdValType signatures = *method_env[key];
    int num_formals_decl = *reinterpret_cast<int*>(signatures[NUM_FORMALS]);
    if (num_formals_decl != actual->len()) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Method " << name << " called with wrong number of arguments.\n";
        return Object;
    }
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol Ti = typecheck_expr(actual->nth(i));
        Symbol Ti_decl = signatures[i];
        if (!conform(Ti, Ti_decl)) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "In call of method " << name << ", type " << Ti
                << " does not conform to declared type " << Ti_decl << ".\n";
            return Object;
        }
    }
    Symbol T_ret = signatures[actual->len()];
    if (semant_debug)
        cout << "Method " << name << " return type: " << T_ret << " -> ";
    T_ret = (T_ret == SELF_TYPE) ? T0 : T_ret;
    if (semant_debug)
        cout << T_ret << endl;
    expr->set_type(T_ret);
    return T_ret;
}

Symbol ClassTable::typecheck_cond(Expression expr) {
    cond_class *e = dynamic_cast<cond_class*>(expr);
    Symbol T1 = typecheck_expr(e->get_pred());
    Symbol T2 = typecheck_expr(e->get_then());
    Symbol T3 = typecheck_expr(e->get_else());
    if (T1 != Bool) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Predicate of 'if' does not have type Bool (Got: "
            << T1 << ")." << endl;
        return Object;
    }
    expr->set_type(lub(T2, T3));
    return expr->get_type();
}

Symbol ClassTable::typecheck_block(Expression expr) {
    block_class *e = dynamic_cast<block_class*>(expr);
    obj_env.enterscope();
    Expressions exprs = e->get_body();
    Symbol T_tmp = Object;
    for (int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
        T_tmp = typecheck_expr(exprs->nth(i));
    }
    obj_env.exitscope();
    expr->set_type(T_tmp);
    return T_tmp;
}

Symbol ClassTable::typecheck_let(Expression expr) {
    let_class *e = dynamic_cast<let_class*>(expr);
    Symbol T0 = e->get_type();
    T0 = (T0 == SELF_TYPE) ? SELF_TYPE : T0;
    if (T0 != SELF_TYPE && !ig_nodes.count(T0)) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Class " << e->get_type()
            << " of let-bound identifier " << e->get_iden()
            << " is undefined." << endl;
        return Object;
    } 
    Expression init_expr = e->get_init();

    // Let-No-Init
    if (dynamic_cast<no_expr_class*>(init_expr)) {
        obj_env.enterscope();
        if (e->get_iden() == self) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "'self' cannot be bound in a 'let' expression." << endl;
            return Object;
        }
        obj_env.addid(e->get_iden(), new Symbol(T0));
        Symbol body_type = typecheck_expr(e->get_body());
        obj_env.exitscope();
        init_expr->set_type(No_type);
        expr->set_type(body_type);
        return body_type;
    }

    // Let-Init
    // First check init expr without definition of new identifier
    Symbol init_type = typecheck_expr(e->get_init());
    if (!conform(init_type, T0)) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Inferred type " << init_type
            << " of initialization of " << e->get_iden()
            << " does not conform to identifier's declared type "
            << T0 << "." << endl;
        return Object;
    }

    // Then add new identifier and check body
    obj_env.enterscope();
    obj_env.addid(e->get_iden(), new Symbol(T0));
    Symbol body_type = typecheck_expr(e->get_body());
    obj_env.exitscope();
    expr->set_type(body_type);
    return body_type;
}

Symbol ClassTable::typecheck_case(Expression expr) {
    typcase_class *e = dynamic_cast<typcase_class*>(expr);
    Symbol T0 = typecheck_expr(e->get_expr());
    Cases cases = e->get_cases();
    Symbol T_ret;
    std::map<Symbol, bool> case_cache;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        Case c = cases->nth(i);
        obj_env.enterscope();
        if (c->get_type() == SELF_TYPE) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Identifier " << c->get_name()
                << " declared with type SELF_TYPE in case branch." << endl;
            return Object;
        } else if (!ig_nodes.count(c->get_type())) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Class " << c->get_type()
                << " of case branch is undefined." << endl;
            return Object;
        } else if (case_cache.count(c->get_type())) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Duplicate branch " << c->get_type()
                << " in case statement." << endl;
            return Object;
        } else if (c->get_name() == self) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "'self' bound in 'case'." << endl;
            return Object;
        } else {
            case_cache[c->get_type()] = true;
            obj_env.addid(c->get_name(), new Symbol(c->get_type()));
        }
        Symbol Ti = typecheck_expr(c->get_expr());
        T_ret = (i == cases->first()) ? Ti : lub(Ti, T_ret);
        obj_env.exitscope();
    }
    expr->set_type(T_ret);
    return T_ret;
}

Symbol ClassTable::typecheck_loop(Expression expr) {
    loop_class *e = dynamic_cast<loop_class*>(expr);
    if (typecheck_expr(e->get_pred()) != Bool) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Loop condition does not have type Bool." << endl;
        return Object;
    }
    typecheck_expr(e->get_body());
    expr->set_type(Object);
    return Object;
}

Symbol ClassTable::typecheck_isvoid(Expression expr) {
    isvoid_class *e = dynamic_cast<isvoid_class*>(expr);
    typecheck_expr(e->get_expr());
    expr->set_type(Bool);
    return Bool;
}

Symbol ClassTable::typecheck_comp(Expression expr) {
    comp_class *e = dynamic_cast<comp_class*>(expr);
    Symbol T0 = typecheck_expr(e->get_expr());
    if (T0 != Bool) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Argument of 'not' has type " << T0
            << " instead of Bool." << endl;
        return Object;
    }
    expr->set_type(Bool);
    return Bool;
}

Symbol ClassTable::typecheck_compare(Expression expr) {
    Symbol T_lhs, T_rhs;
    bool is_lt = false;
    if (dynamic_cast<lt_class*>(expr)) {
        lt_class *e = dynamic_cast<lt_class*>(expr);
        T_lhs = typecheck_expr(e->get_lhs());
        T_rhs = typecheck_expr(e->get_rhs());
        is_lt = true;
    } else {
        leq_class *e = dynamic_cast<leq_class*>(expr);
        T_lhs = typecheck_expr(e->get_lhs());
        T_rhs = typecheck_expr(e->get_rhs());
    }
    if (T_lhs != Int || T_rhs != Int) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "non-Int arguments: " << T_lhs
            << (is_lt ? " < " : " <= ") << T_rhs << endl;
        return Object;
    }
    expr->set_type(Bool);
    return Bool;
}

Symbol ClassTable::typecheck_neg(Expression expr) {
    neg_class *e = dynamic_cast<neg_class*>(expr);
    Symbol T0 = typecheck_expr(e->get_expr());
    if (T0 != Int) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Argument of '~' has type " << T0
            << " instead of Int." << endl;
        return Object;
    }
    expr->set_type(Int);
    return Int;
}

Symbol ClassTable::typecheck_arith(Expression expr) {
    Symbol T1, T2;
    char *display;
    if (dynamic_cast<plus_class*>(expr)) {
        plus_class *e = dynamic_cast<plus_class*>(expr);
        T1 = typecheck_expr(e->get_e1());
        T2 = typecheck_expr(e->get_e2());
        display = " + ";
    } else if (dynamic_cast<sub_class*>(expr)) {
        sub_class *e = dynamic_cast<sub_class*>(expr);
        T1 = typecheck_expr(e->get_e1());
        T2 = typecheck_expr(e->get_e2());
        display = " - ";
    } else if (dynamic_cast<mul_class*>(expr)) {
        mul_class *e = dynamic_cast<mul_class*>(expr);
        T1 = typecheck_expr(e->get_e1());
        T2 = typecheck_expr(e->get_e2());
        display = " * ";
    } else {
        divide_class *e = dynamic_cast<divide_class*>(expr);
        T1 = typecheck_expr(e->get_e1());
        T2 = typecheck_expr(e->get_e2());
        display = " / ";
    }
    if (T1 != Int || T2 != Int) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "non-Int arguments: " 
            << T1 << display << T2 << endl;
        return Object;
    }
    expr->set_type(Int);
    return Int;
}

Symbol ClassTable::typecheck_equal(Expression expr) {
    eq_class *e = dynamic_cast<eq_class*>(expr);
    Symbol T_lhs = typecheck_expr(e->get_lhs());
    Symbol T_rhs = typecheck_expr(e->get_rhs());
    if (is_prim_type(T_lhs) || is_prim_type(T_rhs)) {
        if (T_lhs != T_rhs) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Illegal comparision with a basic type." << endl;
            return Object;
        }
    }
    expr->set_type(Bool);
    return Bool;
}

void ClassTable::typecheck_method(Feature feat) {
    if (semant_debug) cout << "Method: " << feat->get_name() << endl;
    MtdKeyType key = std::make_pair(cls_env->get_name(), feat->get_name());
    MtdKeyType key_search = key;
    bool defined = true;
    Symbol T_search = cls_env->get_name();
    Symbol name = feat->get_name();
    if (T_search == Object || T_search == IO || is_prim_type(T_search))
        defined = false;
    else {
        while(!method_env.count(key_search)) {
            if (semant_debug)
                cout <<  "[INFO] Declaring " << name << ", not found in class "
                    << T_search << ", searching parent..." << endl;
            if (T_search == Object) {
                defined = false;
                break;
            }
            T_search = ig_nodes[T_search]->get_parent();
            key_search = std::make_pair(T_search, name);
        }
    }
    if (defined) {
        // multiply defined within same class
        if (T_search == cls_env->get_name()) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Method " << name
                << " is multiply defined." << endl;
            return;
        }
        if (semant_debug)
            cout << "[INFO] " << name << " found in "
                << T_search << endl;
        // Overriding parent, check signature conformance
        MtdValType &signatures = *method_env[key_search];
        _check_formal_conformance(feat, signatures);
    } else {
        // New method
        method_env[key] = new MtdValType();
        int num_formals = _add_formal_signatures(feat);
        MtdValType &signatures = *method_env[key];
        // check method return type defined
        Symbol return_type =feat->get_type();
        if (return_type != SELF_TYPE && !ig_nodes.count(return_type)) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Undefined return type " << return_type   
                << " in method " << feat->get_name() << "." << endl;
            signatures[num_formals] = Object;
        } else 
            signatures[num_formals] = return_type;
    }
}

// One attr initialization expression may reference other attr,
// we need two passes, in first pass we only add id.
void ClassTable::typecheck_attr(Feature feat, bool addid_only) {
    if (semant_debug) {
        if (addid_only)
            cout << "Attr addid " << feat->get_name() << endl;
        else
            cout << "Attr typecheck init " << feat->get_name() << endl;
    }
    Symbol name = feat->get_name();
    Symbol type_decl = feat->get_type();
    Expression init = feat->get_expr();
    // check 'self'
    if (name == self) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "'self' cannot be the name of an attribute." << endl;
        return;
    }
    // check overrite parent attribute when adding id
    if (obj_env.lookup(name) && addid_only == true) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << " Attribute " << name 
            << " is an attribute of an inherited class." << endl;
        return;
    }
    // check attr type defined
    if (type_decl != SELF_TYPE && !ig_nodes.count(type_decl)) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Class " << type_decl
            << " of attribute " << name
            << " is undefined." << endl;
        return;
    } 
    if (addid_only) {
        obj_env.addid(name, new Symbol(type_decl));
        return;
    }
    if (dynamic_cast<no_expr_class*>(init)) {
        init->set_type(No_type);
    } else {
        // handle attr init expressions
        obj_env.enterscope();
        Symbol T0 = type_decl;
        obj_env.addid(self, new Symbol(SELF_TYPE));
        Symbol T1 = typecheck_expr(init);
        if (!conform(T1, T0)) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Inferred type " << T1
                << " of initialization of attribute " << name
                << " does not conform to declared type " 
                << T0 << "." << endl;
        }
        obj_env.exitscope();
    }
}

/*** end of type checker ***/

bool ClassTable::is_prim_type(Symbol sym) {
    for (int i = 0; i != NUM_PRIMITIVES; ++i)
        if (sym == prim_types[i]) return true;
    return false;
}

void ClassTable::_find_main(Classes classes) {
    bool has_main = false;
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
        if (classes->nth(i)->get_name() == Main) {
            has_main = true;
            break;
        }
    if (!has_main) {
        semant_error();
        cerr << "Class Main is not defined." << endl;
    }
}

bool ClassTable::conform(Symbol lhs, Symbol rhs) {
    if (lhs == rhs) return true;
    if (lhs == SELF_TYPE) return conform(cls_env->get_name(), rhs);
    if (rhs == SELF_TYPE) return false;

    Class_ c_lhs = ig_nodes[lhs];
    while(c_lhs->get_name() != rhs && c_lhs->get_name() != Object) {
        c_lhs = ig_nodes[c_lhs->get_parent()];
    }
    return (c_lhs->get_name() == rhs) ? true : false;
}

Symbol ClassTable::lub(Symbol a, Symbol b) {
    if (a == b) return a;
    if (a == SELF_TYPE) return lub(cls_env->get_name(), b);
    if (b == SELF_TYPE) return lub(a, cls_env->get_name());

    List<Symbol> *list_a = NULL, *list_b = NULL;
    Class_ c = ig_nodes[a];
    while(c->get_name() != Object) {
        list_a = new List<Symbol>(new Symbol(c->get_name()), list_a);
        c = ig_nodes[c->get_parent()];
    }
    list_a = new List<Symbol>(new Symbol(Object), list_a);
    c = ig_nodes[b];
    while(c->get_name() != Object) {
        list_b = new List<Symbol>(new Symbol(c->get_name()), list_b);
        c = ig_nodes[c->get_parent()];
    }
    list_b = new List<Symbol>(new Symbol(Object), list_b);

    Symbol ret;
    for (;
            list_a != NULL && list_b != NULL; 
            list_a = list_a->tl(), list_b = list_b->tl()
            ) {
        if (*list_a->hd() == *list_b->hd())
            ret = *list_a->hd();
    }
    return ret;
}

void ClassTable::_check_formal_conformance(
        Feature feat, MtdValType &signatures) {
    Formals formals = feat->get_formals();
    // Check num args
    if (*reinterpret_cast<int*>(signatures[NUM_FORMALS]) 
            != formals->len()) {
        semant_error();
        dump_fname_lineno(cerr, cls_env)
            << "Incompatible number of formal parameters in "
            "redefined method " << feat->get_name() << "." << endl;
        return;
    }

    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal form = formals->nth(i);
        Symbol param = form->get_name();
        if (semant_debug)
            cout << " Check Formal conformance: " << param << ", type: "
                << form->get_type() << endl;
        if (form->get_type() != signatures[i]) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "In redefined method " << feat->get_name()
                << ", parameter type " << form->get_type() 
                << " is different from original type "
                << signatures[i] << endl;
        }
    }
}

int ClassTable::_add_formal_signatures(Feature feat) {
    Formals formals = feat->get_formals();
    MtdKeyType key = std::make_pair(cls_env->get_name(), feat->get_name());
    MtdValType &signatures = *method_env[key];
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal form = formals->nth(i);
        Symbol param = form->get_name();
        if (semant_debug)
            cout << " Check Formal signature: " << param << ", type: "
                << form->get_type() << endl;
        // 'self' as formal name
        if (param == self) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "'self' cannot be the name of a formal parameter" << endl;
        }
        // Undefined formal type
        if (!ig_nodes.count(form->get_type())) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Class " << form->get_type()
                << " of formal paramer " << param
                << " is undefined." << endl;
            signatures[i] = Object;
        } else 
            signatures[i] = form->get_type();
    }
    signatures[formals->len()] = feat->get_type();
    signatures[NUM_FORMALS] = reinterpret_cast<Symbol>(new int(formals->len()));  
    return formals->len();
}

int ClassTable::_add_formal_ids(Feature feat) {
    Formals formals = feat->get_formals();
    int i = 0;
    std::map<Symbol, bool> param_cache;
    for (i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal form = formals->nth(i);
        Symbol param = form->get_name();
        if (semant_debug)
            cout << " Add Formal id: " << param << ", type: "
                << form->get_type() << endl;
        if (param_cache.count(param)) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Formal parameter " << param
                << " is multiply defined." << endl;
        } else 
            param_cache[param] = true;
        obj_env.addid(param, new Symbol(form->get_type()));
    }
    return i;
}

void ClassTable::_check_method_body(Class_ cls) {
    obj_env = obj_env_cache[cls->get_name()];
    cls_env = cls;

    if (semant_debug)  cout << "Method checking: " << cls->get_name() << endl;
    Features features = cls->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        if (f->is_method()) {
            // handle expression body
            if (semant_debug) cout << " Method: " << f->get_name() << endl;
            obj_env.enterscope();
            obj_env.addid(self, new Symbol(SELF_TYPE));
            _add_formal_ids(f);
            Symbol T0 = f->get_type();
            Symbol T0_infer = typecheck_expr(f->get_expr()); // return type, T0'
            if (!conform(T0_infer, T0)) {
                semant_error();
                dump_fname_lineno(cerr, cls)
                    << "Inferred return type " << T0_infer
                    << " of method " << f->get_name() 
                    << " does not conform to declared return type "
                    << T0 << "." << endl;
            }
            obj_env.exitscope();
        }
    }
}

void ClassTable::_decl_attrs(Class_ cls) {
    obj_env.enterscope();
    cls_env = cls;

    if (semant_debug)  cout << "Attr adding: " << cls->get_name() << endl;
    Features features = cls->get_features();
    // first pass: addid
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        if (f->is_method()) continue;
        if (is_prim_type(cls_env->get_name())) continue;
        typecheck_attr(f, true);
    }
    // second pass: addid
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        if (f->is_method()) continue;
        if (is_prim_type(cls_env->get_name())) continue;
        typecheck_attr(f, false);
    }

    // Then, dfs step, add sub class attrs / attr-expr
    for (IGnode_list *l = cls->get_children(); l != NULL; l = l->tl()) {
        Class_ child = l->hd();
        _decl_attrs(child);
    }
    obj_env_cache[cls->get_name()] = obj_env;  // save current scope
    obj_env.exitscope();
}

void ClassTable::_decl_methods(Class_ cls) {
    cls_env = cls;

    // First add methods
    if (semant_debug)  cout << "Declaring class: " << cls->get_name() << endl;
    Features features = cls->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        if (f->is_method())
            typecheck_method(f);
    }

    // Then, dfs step, add sub class method signature
    for (IGnode_list *l = cls->get_children(); l != NULL; l = l->tl()) {
        Class_ child = l->hd();
        _decl_methods(child);
    }
}

bool ClassTable::_check_inheritance_graph() {
    // Connect every IGnode(Class_) to its children
    typedef std::map<Symbol, Class_>::iterator _Iter;
    for (_Iter iter = ig_nodes.begin(); iter != ig_nodes.end(); ++iter) {
        Class_ node = iter->second;
        // Skip Object
        if (node->get_name() == Object) continue;

        Symbol parent = node->get_parent();
        // Make sure it does not inherit from primitive types
        if (is_prim_type(parent)) {
            semant_error();
            dump_fname_lineno(cerr, node)
                << "Class " << node->get_name()
                << " cannot inherits class " 
                << parent << "." << endl;
            return false;
        }
        // Check inherit from undefined
        if (!ig_nodes.count(parent)) {
            semant_error();
            dump_fname_lineno(cerr, node)
                << "Class " << node->get_name()
                << " inherits from an undefined class " 
                << parent << "." << endl;
            return false;
        } 
        Class_ parent_node = ig_nodes[parent];
        parent_node->add_children(node);
        if (semant_debug) {
            cout << "Adding: " << node->get_name() << " to child of " 
                << parent_node->get_name()<< endl;
        }
    }

    // BFS
    std::map<Class_, bool> visited;
    std::deque<Class_> que;
    que.push_back(Object_node); // start bfs from root
    while(!que.empty()) {
        Class_ node = que.front();
        if (semant_debug)
            cout << "BFSing: " << node->get_name() << endl;
        que.pop_front();
        visited[node] = true;

        for (IGnode_list *l = node->get_children(); l != NULL; l = l->tl()) {
            Class_ child = l->hd();
            // No need to check multiple inheritance, its done by Grammar/parser
            que.push_back(child);
        }
    }
    // For those no visited by bfs, cycle must occurs.
    bool flag = true;
    for (_Iter iter = ig_nodes.begin(); iter != ig_nodes.end(); ++iter) {
        Class_ node = iter->second;
        if (visited.count(node)) continue;
        else {
            semant_error();
            dump_fname_lineno(cerr, node)
                << "Class " << node->get_name() 
                << ", or an ancestor of " << node->get_name()
                << ", is involved in an inheritance cycle." << endl;
            flag = false;
        }
    }

    return flag;
}

void ClassTable::_check_ignodes(Classes classes) {
    if (semant_debug)
        cout <<  "Num classes: " << classes->len() << endl;
    // Gather class infos into ig_nodes, 1st pass
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ cur = classes->nth(i);
        if (semant_debug) {
            cout << "Gathering: " << cur->get_name() << endl;
        }
        if (ig_nodes.count(cur->get_name())) {
            // conflict with prim types?
            bool redefine_prim = false;
            if (is_prim_type(cur->get_name())) {
                redefine_prim = true;
                semant_error();
                dump_fname_lineno(cerr, cur)
                    << "Redefinition of basic class " 
                    << cur->get_name() << endl;
            }
            // multiple definition?
            if (!redefine_prim) {
                semant_error();
                dump_fname_lineno(cerr, cur)
                    << "Class " << cur->get_name()
                    << " was previously defined." << endl;
            }
        } else if (cur->get_name() == SELF_TYPE) {
            semant_error();
            dump_fname_lineno(cerr, cur)
                << "Redefinition of basic class SELF_TYPE." << endl;
        } else 
            ig_nodes[cur->get_name()] = cur;
    }
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */
    obj_env = ObjEnvType();
    method_env = MtdEnvType();
    obj_env_cache = ObjCacheType();
    cls_env = NULL;

    // Pass I: Check inheritance graph()
    install_basic_classes();
    _check_ignodes(classes);
    if (!_check_inheritance_graph()) return;  // second pass
        
    // Pass II: DFS inheritance graph from root,  adding method declaration.
    if (semant_debug) cout << endl << "Second pass: _decl_methods()" << endl;
    _decl_methods(Object_node);

    // Pass III, check/add attr
    if (semant_debug) cout << endl << "Third pass: _decl_attrs()" << endl;
    _decl_attrs(Object_node);
    
    // Pass IV, check method expr
    if (semant_debug) 
        cout << endl << "Fourth pass: _check_method_body()" << endl;
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
        _check_method_body(classes->nth(i));

    // Check if 'Main' defined
    _find_main(classes);
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    // Register to ig_nodes
    Object_node = Object_class;
    ig_nodes[Object] = Object_node;
    ig_nodes[IO] = IO_class;
    ig_nodes[Int] = Int_class;
    ig_nodes[Str] = Str_class;
    ig_nodes[Bool] = Bool_class;
    prim_types = new Symbol[NUM_PRIMITIVES];
    prim_types[0] = Int, prim_types[1] = Str, prim_types[2] = Bool;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


