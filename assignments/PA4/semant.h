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

/* MtdValType: (T1, T2, ..., TN, TN+1)
 * MtdValType[NUM_FORMALS] = N            */

class ClassTable {
private:
  static const int NUM_PRIMITIVES;
  /* An arbitrary large number to store number of formals in MtdValType. */
  static const int NUM_FORMALS; 

  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  // TypeEnv
  typedef SymbolTable<Symbol, Symbol> ObjEnvType;  // Object env
  typedef std::map<Symbol, ObjEnvType> ObjCacheType;
  typedef std::pair<Symbol, Symbol> MtdKeyType;
  typedef std::map<int, Symbol>     MtdValType;
  typedef std::map<MtdKeyType, MtdValType*> MtdEnvType; // Method env
  ObjEnvType obj_env;
  MtdEnvType method_env;
  ObjCacheType obj_env_cache;
  Class_   cls_env;

  // Helper function
  bool is_prim_type(Symbol);
  void _find_main(Classes classes);
  void _check_formal_conformance(Feature, MtdValType &);
  int _add_formal_signatures(Feature);
  int _add_formal_ids(Feature);
  void _check_method_body(Class_);
  void _decl_attrs(Class_);
  void _decl_methods(Class_);
  bool _check_inheritance_graph();
  void _check_ignodes(Classes);

  // Type cheker
  Symbol typecheck_expr(Expression);
  Symbol typecheck_var(Expression);
  Symbol typecheck_assign(Expression);
  Symbol typecheck_bool(Expression);
  Symbol typecheck_int(Expression);
  Symbol typecheck_string(Expression);
  Symbol typecheck_new(Expression);
  Symbol typecheck_dispatch(Expression);
  Symbol typecheck_static_dispatch(Expression);
  Symbol typecheck_cond(Expression);
  Symbol typecheck_block(Expression);
  Symbol typecheck_let(Expression);
  Symbol typecheck_case(Expression);
  Symbol typecheck_loop(Expression);
  Symbol typecheck_isvoid(Expression);
  Symbol typecheck_comp(Expression);
  Symbol typecheck_compare(Expression);
  Symbol typecheck_neg(Expression);
  Symbol typecheck_arith(Expression);
  Symbol typecheck_equal(Expression);
  void typecheck_method(Feature);
  void typecheck_attr(Feature, bool);

  Class_ Object_node;                   // Root of inheritance tree
  Symbol *prim_types;                   // Primitive types
  std::map<Symbol, Class_> ig_nodes;    // Inheritance Graph nodes
  bool conform(Symbol, Symbol);         // type conformance
  Symbol lub(Symbol, Symbol);           // least upper bound

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};
const int ClassTable::NUM_PRIMITIVES = 3;
const int ClassTable::NUM_FORMALS    = 0x3f3f3f3f;


#endif

