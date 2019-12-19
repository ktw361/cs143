

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


/* Implementation of added function in cool-tree.h */
typedef List<Class__class> IGnode_list;
Symbol class__class::get_name() { return name; }
Symbol class__class::get_parent() { return parent; }
IGnode_list *class__class::get_children() {
    return children;
}
void class__class::add_children(Class_ c) { 
    children = new IGnode_list(c, children); 
}

/* end of implementation in cool-tree.h */

ostream &dump_fname_lineno(ostream &s, Symbol fname, int lineno) {
    s << fname << ":" <<  lineno << ": ";
    return s;
}

bool ClassTable::check_inheritance_graph() {
    // Connect every IGnode(class__class_ to its children
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
                dump_fname_lineno(cerr, node->get_filename(), node->get_line_number())
                    << "Class " << node->get_name()
                    << " cannot inherits class " 
                    << parent << "." << endl;
                return false;
            }
        }
        // Check inherit from undefined
        if (!ig_nodes.count(parent)) {
            dump_fname_lineno(cerr, node->get_filename(), node->get_line_number())
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
                    dump_fname_lineno(
                            cerr, child->get_filename(), child->get_line_number())
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
            dump_fname_lineno(
                    cerr, node->get_filename(), node->get_line_number())
                << "Class " << node->get_name() 
                << ", or an ancestor of " << node->get_name()
                << ", is involved in an inheritance cycle." << endl;
            flag = false;
        }
    }

    return flag;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */
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

    check_inheritance_graph();
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


