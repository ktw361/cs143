

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

/**** end of implementation in cool-tree.h ****/

typedef SymbolTable<Symbol, Symbol> EnvType;
// Can't use plain function, as tree_node doesn't have get_name()
#define dump_fname_lineno(s, n)             \
(s) << (n)->get_filename() << ":" <<  (n)->get_line_number() << ": "

bool ClassTable::check_inheritance_graph() {
    // Connect every IGnode(Class_) to its children
    typedef std::map<Symbol, Class_>::iterator _Iter;
    Symbol prim_types[] = {Int, Bool, Str};
    for (_Iter iter = ig_nodes.begin(); iter != ig_nodes.end(); ++iter) {
        Class_ node = iter->second;
        // Skip Object
        if (node->get_name() == Object) continue;

        Symbol parent = node->get_parent();
        // Make sure it does not inherit from primitive types
        for (int i = 0; i != sizeof(prim_types)/sizeof(Symbol); ++i) {
            if (parent == prim_types[i]) {
                semant_error();
                dump_fname_lineno(cerr, node)
                    << "Class " << node->get_name()
                    << " cannot inherits class " 
                    << parent << "." << endl;
                return false;
            }
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
            if (visited.count(child)) {
                // inheritance cycle
                Symbol cmp = child->get_name();
                while(child->get_parent() != cmp) {
                    semant_error();
                    dump_fname_lineno(cerr, child)
                        << "Class " << child->get_name() 
                        << ", or an ancestor of " << child->get_name()
                        << ", is involved in an inheritance cycle." << endl;
                    child = ig_nodes[child->get_name()];
                }
                return false;
            }
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

void ClassTable::_add_formals(Feature feat) {
    Formals formals = feat->get_formals();
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal form = formals->nth(i);
        if (semant_debug)
            cout << " Formal: " << form->get_name() << ", type: "
                << form->get_type() << endl;
        if (!ig_nodes.count(form->get_type())) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Class " << form->get_type()
                << " of formal paramer " << form->get_name()
                << " is undefined." << endl;
        } else 
            obj_env->addid(form->get_name(), new Symbol(form->get_type()));
    }
}

Symbol ClassTable::typecheck_assign(Symbol id, Expression e) {
    Symbol e_type = typecheck_expr(e);
    // Note the verification of id's type is done by caller.
    Symbol T;
    if ((T = *obj_env->lookup(id)) == NULL) {
        dump_fname_lineno(cerr, cls_env)
            << "Assignment to undeclared variable "
            << id << "." << endl;
        return Object;
    } 
    // TODO
    if (!(e_type <= T)) {
        dump_fname_lineno(cerr, cls_env)
            << "Type " << e_type << " of assigned expression does not"
            << " conform to declared type " << T 
            << " of identifier " << id << "." << endl;
        return Object;
    }
    return e_type;
}

Symbol ClassTable::typecheck_expr(Expression expr) {
    if (dynamic_cast<let_class*>(expr)) {
        let_class *e = dynamic_cast<let_class*>(expr);
        obj_env->enterscope();
        // First check init expr without definition of new identifier
        Symbol init_type = typecheck_expr(e->get_init());

        // Then add new identifier and check body
        obj_env->enterscope();
        if (!ig_nodes.count(e->get_type())) {
            semant_error();
            dump_fname_lineno(cerr, cls_env)
                << "Class " << e->get_type()
                << " of let-bound identifier " << e->get_iden()
                << " is undefined." << endl;
        } else 
            obj_env->addid(e->get_iden(), new Symbol(e->get_type()));
        Symbol body_type = typecheck_expr(e->get_body());
        obj_env->exitscope();

        obj_env->exitscope();
    } else if (dynamic_cast<typcase_class*>(expr)) {
        typcase_class *e = dynamic_cast<typcase_class*>(expr);
        typecheck_expr(e->get_expr());
        Cases cases = e->get_cases();
        for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
            Case c = cases->nth(i);
            obj_env->enterscope();
            if (!ig_nodes.count(c->get_type())) {
                semant_error();
                dump_fname_lineno(cerr, cls_env)
                    << "Class " << c->get_type()
                    << " of case branch is undefined." << endl;
            } else 
                obj_env->addid(c->get_name(), new Symbol(c->get_type()));
            typecheck_expr(c->get_expr());
            obj_env->exitscope();
        }
    } else if (dynamic_cast<block_class*>(expr)) {
        block_class *e = dynamic_cast<block_class*>(expr);
        obj_env->enterscope();
        Expressions exprs = e->get_body();
        for (int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
            typecheck_expr(exprs->nth(i));
        }
        obj_env->exitscope();
    } else {
        ;
    }
}

void ClassTable::_decl_class(Class_ cls) {
    obj_env->enterscope();
    method_env->enterscope();
    cls_env = cls;

    if (semant_debug)  cout << "Declaring class: " << cls->get_name() << endl;
    Features features = cls->get_features();
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        Symbol name = f->get_name();
        if (f->is_method()) {
            if (semant_debug) cout << "Method: " << name << endl;
            // handle formals
            _add_formals(f);
            // check return type defined
            bool defined = ig_nodes.count(f->get_type());
            if (!defined) {
                semant_error();
                dump_fname_lineno(cerr, cls)
                    << "Undefined return type " << f->get_type()
                    << " in method " << name << "." << endl;
            } 
            // handle expressions
            obj_env->enterscope();
            typecheck_expr(f->get_expr());
            obj_env->exitscope();
            if (defined)
                method_env->addid(name, new Symbol(f->get_type()));
        } else {
            if (semant_debug) cout << "Attr: " << name << endl;
            // check attr type defined
            bool defined = ig_nodes.count(f->get_type());
            if (!defined) {
                semant_error();
                dump_fname_lineno(cerr, cls)
                    << "Class " << f->get_type()
                    << " of attribute " << name
                    << " is undefined." << endl;
                obj_env->addid(name, new Symbol(Object));
            } else
                obj_env->addid(name, new Symbol(f->get_type()));
            // handle init expressions
            obj_env->enterscope();
            typecheck_assign(f->get_name(), f->get_expr());
            obj_env->exitscope();
        }
    }
    obj_env->exitscope();
    method_env->exitscope();
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */
    obj_env = new SymbolTable<Symbol, Symbol>();
    method_env = new SymbolTable<Symbol, Symbol>();
    cls_env = NULL;

    // Part I: Check inheritance graph()
    install_basic_classes();
    if (semant_debug)
        cout <<  "Num classes: " << classes->len() << endl;
    // Gather class infos into ig_nodes, 1st pass
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ cur = classes->nth(i);
        if (semant_debug) {
            cout << "Gathering: " << cur->get_name() << endl;
        }
        if (ig_nodes.count(cur->get_name())) {
            // multiple definition
            semant_error();
            cerr << cur->get_filename() << ":";
            cerr << cur->get_line_number() << ": ";
            cerr << "Class " << cur->get_name();
            cerr << " was previously defined." << endl;
        } else 
            ig_nodes[cur->get_name()] = cur;
    }
    if (!check_inheritance_graph()) return;  // second pass

    Symbol F2 = idtable.add_string("F2");
    Symbol FF = idtable.add_string("FF");
    Symbol C = idtable.add_string("C");
    Symbol A = idtable.add_string("A");
    Symbol F1 = idtable.add_string("F1");
    cout << conform(Int, Object) 
        << conform(Object, Object) 
        << conform(F2, Object) 
        << conform(F2, F1)
        << conform(F1, F2)
        << conform(F1, C)
        << conform(F2, C) << endl;
    cout << lub(Object, Object) << endl
        << lub(Int, Bool) << endl
        << lub(F1, F2) << endl
        << lub(FF, A) << endl;
        
    // Part II: add declaration to symbol table; check expression correctness.
    /* for (int i = classes->first(); classes->more(i); i = classes->next(i)) { */
    /*     _decl_class(classes->nth(i)); */
    /* } */
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


