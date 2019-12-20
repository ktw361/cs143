#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <map>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  // Symbol table
  SymbolTable<Symbol, Symbol> *obj_env;
  SymbolTable<Symbol, Symbol> *method_env;
  // Helper function
  void _decl_class(Class_);
  void _add_formals(Feature, Class_);
  void _add_expr(Expression, Class_);

  Class_ Object_node;
  std::map<Symbol, Class_> ig_nodes;  // Inheritance Graph nodes
  bool check_inheritance_graph();

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

