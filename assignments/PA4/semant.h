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
  // TypeEnv
  typedef SymbolTable<Symbol, Symbol> EnvType;
  EnvType *obj_env;
  EnvType *method_env;
  Class_   cls_env;

  // Helper function
  void _decl_class(Class_);
  void _add_formals(Feature);
  // Type cheker
  Symbol typecheck_expr(Expression);
  Symbol typecheck_assign(Symbol, Expression);

  Class_ Object_node;  // Root of inheritance tree
  std::map<Symbol, Class_> ig_nodes;    // Inheritance Graph nodes
  bool check_inheritance_graph();
  bool conform(Symbol, Symbol);         // type conformance
  Symbol lub(Symbol, Symbol);           // least upper bound

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

