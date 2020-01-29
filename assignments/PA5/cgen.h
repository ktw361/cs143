#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

#define TAGS 6
#define OBJECT_SIZE 3

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

struct DispTabEntry;
typedef DispTabEntry *DispTabEntryP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int objectclasstag;  // 0
   int ioclasstag;      // 1
   int intclasstag;     // 2
   int boolclasstag;    // 3
   int stringclasstag;  // 4
   int mainclasstag;    // 5
   int tags;          // global counter


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   void code_proto_obj();
   void code_disp_table();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void set_classes_size();
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int _tag;
   int _objsize;
   int _num_methods;

   typedef SymbolTable<Symbol, int> OffsetT;
   typedef SymbolTable<int, DispTabEntry> TableT;
   OffsetT *disp_offset;
   TableT *disp_tab;
   int* probe_offset(Symbol m);
   DispTabEntryP probe_entry(int offset);

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() const { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() const { return parentnd; }
   int basic() const { return (basic_status == Basic); }

   void set_tag(int tag) { _tag = tag; }
   void set_size(int sz) { _objsize = sz; }
   int tag() const { return _tag; }
   int size() const { return _objsize; }
   int num_methods() const { return _num_methods; }
   int num_attrs() const;
   void code_attrs(ostream&) const;
   void build_disptab(ostream&);
   void code_disptab(ostream&) const;
};

// dispatch info
struct DispTabEntry {
  Symbol cls;
  Symbol method;
  DispTabEntry(Symbol c, Symbol m) : cls(c), method(m) {}
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

