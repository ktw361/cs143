
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <deque>

#include "cgen.h"
#include "cgen_gc.h"

#define LOOP_LIST_NODE(i, list) \
  for (int i = (list)->first(); (list)->more(i); i = (list)->next(i))

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

// TODO check necessary
static CgenClassTableP cgen_classtable; // staticDisp and new__class
static CgenNodeP cur_cgnode;
static EnvType *cur_env;
static int fp_offset = 0; // in words

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_truebool(char *dest, ostream& s)
{
  emit_partial_load_address(dest, s);
  truebool.code_ref(s); 
  s << endl;
}

static void emit_load_falsebool(char *dest, ostream& s)
{
  emit_partial_load_address(dest, s);
  falsebool.code_ref(s); 
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_load_disptable(char *dest_reg, Symbol sym, ostream& s)
{ 
  emit_partial_load_address(dest_reg, s); 
  emit_disptable_ref(sym, s); s << endl;
}

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_jal_init_ref(Symbol sym, ostream& s)
{ s << JAL << sym << CLASSINIT_SUFFIX << endl; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_load_protobj_ref(char *dest_reg, Symbol sym, ostream& s)
{
  emit_partial_load_address(dest_reg, s);
  emit_protobj_ref(sym, s);
  s << endl;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_method_def(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname << LABEL; }

static void emit_jal_method(Symbol classname, Symbol methodname, ostream& s)
{ s << JAL << classname << METHOD_SEP << methodname << endl; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Pop top of stack to a register. The stack shrinks towards larger addresses.
//
static void emit_pop(char *reg, ostream& str)
{
  emit_load(reg,1,SP,str);
  emit_addiu(SP,SP,4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << STRINGNAME << DISPTAB_SUFFIX;
      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/

      s << INTNAME << DISPTAB_SUFFIX;
      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << BOOLNAME << DISPTAB_SUFFIX;
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

// 
// Emit code for class_nameTab
//
void CgenClassTable::code_name_table()
{
  // emit class_nameTab
  str << CLASSNAMETAB << LABEL;
  for (int i = 0; i != tags; ++i) {
    Symbol name = name_tab->probe(i);
    str << WORD;
    stringtable.lookup_string(name->get_string())->code_ref(str);
    str << endl;
  }
}

// Emint code for class_objTab
//
void CgenClassTable::code_obj_table()
{
  // emit class_objTab
  str << CLASSOBJTAB << LABEL;
  for (int i = 0; i != tags; ++i) {
    Symbol name = name_tab->probe(i);
    str << WORD; emit_protobj_ref(name, str); str << endl
      << WORD; emit_init_ref(name, str); str << endl;
  }
}

//
// Emit code for proto objects
//
void CgenClassTable::code_proto_obj()
{
  CgenNodeP tree_root = root();
  std::deque<CgenNodeP> que;
  que.push_back(tree_root);

  while(!que.empty()) {
    CgenNodeP nd = que.front();
    que.pop_front();

    str << WORD << "-1" << endl;
    emit_protobj_ref(nd->get_name(), str);
    str << LABEL
      << WORD << nd->tag() << endl
      << WORD << nd->size() << endl
      << WORD; emit_disptable_ref(nd->get_name(), str); str << endl;
    nd->code_attrs(str);

    for (List<CgenNode> *l = nd->get_children(); l; l = l->tl())
      que.push_back(l->hd());
  }
}

//
// Emit code for each class's dispatch table
//
void CgenClassTable::code_disptabs() {
  // dfs perserve inheritance order
  CgenNodeP tree_root = root();
  tree_root->build_disptab(str);
}

//
// Emit code for each class's initialization label definition
//
void CgenClassTable::code_inits() {
  for(List<CgenNode> *l = nds; l; l = l->tl())
    l->hd()->code_init(str);
}

// 
// Emit code for each class's method definition
//
void CgenClassTable::code_method_defs() {
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    Symbol cls_name = l->hd()->get_name(); 
    if (cls_name == Object ||
        cls_name == IO ||
        cls_name == Str)
      continue;
    if (cgen_debug) cout << "coding class " << cls_name << " method" << endl;
    l->hd()->code_method_def(str);
  }
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : 
  nds(NULL) , str(s), name_tab(new NameTabT())
{
   objectclasstag = 0;
   ioclasstag     = 1;
   intclasstag =    2 /* Change to your Int class tag here */;
   boolclasstag =   3 /* Change to your Bool class tag here */;
   stringclasstag = 4 /* Change to your String class tag here */;
   mainclasstag   = 5;

   cgen_classtable = this;
   enterscope();
   name_tab->enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   build_class_attrtab();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  LOOP_LIST_NODE(i, cs)
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  tags = TAGS;
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
      CgenNodeP nd = l->hd();
      set_relations(nd);

      // class name is coded into idtable in ast-lex.cc
      Symbol name = nd->get_name();
      if (name == Object) 
        nd->set_tag(objectclasstag);
      else if (name == IO) 
        nd->set_tag(ioclasstag);
      else if (name == Main)
        nd->set_tag(mainclasstag);
      else if (name == Bool)
        nd->set_tag(boolclasstag);
      else if (name == Int)
        nd->set_tag(intclasstag);
      else if (name == Str)
        nd->set_tag(stringclasstag);
      else {
        nd->set_tag(tags++);
      }
      name_tab->addid(nd->tag(), name);
      if (cgen_debug) {
        cout << "Generate tag for: " << nd->tag() << ", " << name << endl;
      }
  }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

//
// CgenClassTable::build_class_attrtab
//
// Build attr table and calcuate size field for each class
//
void CgenClassTable::build_class_attrtab() 
{
  if (cgen_debug)
    cout << "Building class attribute table" << endl;
  CgenNodeP tree_root = root();
  tree_root->build_attrtab();
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  // class_nameTab and class_objTab
  if (cgen_debug) cout << "coding name table" << endl;
  code_name_table();
  if (cgen_debug) cout << "coding object table" << endl;
  code_obj_table();

  // dispatch_table
  if (cgen_debug) cout << "coding dispatch table" << endl;
  code_disptabs();
  // emit prototype objects for each class
  if (cgen_debug) cout << "coding proto object" << endl;
  code_proto_obj();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  if (cgen_debug) cout << "coding object initializer" << endl;
  code_inits();
  if (cgen_debug) cout << "coding class methods" << endl;
  code_method_defs();

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus),
   _tag(0),
   _num_methods(0),
   attr_tab(new AttrTableT()),
   disp_offset(new OffsetT()),
   disp_tab(new TableT())
   // TODO add new fields
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

// Build attribute table recursively
void CgenNode::build_attrtab() {
  attr_tab->enterscope();
  Features feats = features;

  if (name == Object) {
    _num_attrs = DEFAULT_OBJFIELDS;
    // dfs step
    for (List<CgenNode> *l = children; l; l = l->tl())
      l->hd()->build_attrtab();
    return;
  }

  AttrTableT *parent_attrs = parentnd->attr_tab;
  _num_attrs = parentnd->num_attrs();

  LOOP_LIST_NODE(i, feats)
  {
    attr_class *attr = dynamic_cast<attr_class*>(feats->nth(i));
    if (attr != NULL) {
      // semant guarantees that attr name will not conflict with parent's
      // thus no need to do lookup/probe anymore
      attr_tab->addid(attr->name, new int(_num_attrs));
      _num_attrs++;
    }
  }

  // dfs step
  for (List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->build_attrtab();
}

// Emit all attribute fields during *_protoObj definition
void CgenNode::code_attrs(ostream &str) const {
  // Handle basic protoObj
  if (name == Object) return;
  if (name == Int || name == Bool) {
    str << WORD << "0" << endl;
    return;
  }
  if (name == Str) {
    IntEntryP int_default = inttable.lookup_string("0");
    str << WORD; int_default->code_ref(str); str << endl;
    str << WORD << "0" << endl;
    return;
  }

  // Handle non-basic protObj
  get_parentnd()->code_attrs(str);
  Features feats = features;
  LOOP_LIST_NODE(i, feats)
  {
    attr_class* attr = dynamic_cast<attr_class*>(feats->nth(i));
    if (attr != NULL) {
      Symbol type = attr->type_decl;
      if (type == Int) {
        IntEntryP int_default = inttable.lookup_string("0");
        str << WORD; int_default->code_ref(str); str << endl;
      } else if (type == Bool) {
        str << WORD; falsebool.code_ref(str); str << endl;
      } else if (type == Str) {
        StringEntryP str_default = stringtable.lookup_string("");
        str << WORD; str_default->code_ref(str); str << endl;
      } else {
        // "void" for other classes
        str << WORD << "0" << endl;
      }
    }
  }
}

// Build disptab table recursively,
// and calls code_disptab() to emit dispatch table code.
void CgenNode::build_disptab(ostream &str) {
  disp_offset->enterscope();
  disp_tab->enterscope();
  Features feats = features;

  if (name == Object) {
    LOOP_LIST_NODE(i, feats)
    {
      method_class *method = dynamic_cast<method_class*>(feats->nth(i));
      if (method != NULL) {
        Symbol method_name = method->name;
        DispTabEntryP entry = new DispTabEntry(name, method_name);
        disp_offset->addid(method_name, new int(_num_methods));
        disp_tab->addid(_num_methods, entry);
        _num_methods++;
      } // end if 
    } // end for

    code_disptab(str); // emit code

    // dfs step
    for (List<CgenNode> *l = children; l; l = l->tl())
      l->hd()->build_disptab(str);
    return;
  }

  OffsetT *parent_offset = parentnd->disp_offset;
  TableT *parent_tab = parentnd->disp_tab;
  _num_methods = parentnd->num_methods();

  LOOP_LIST_NODE(i, feats)
  {
    method_class *method = dynamic_cast<method_class*>(feats->nth(i));
    if (method != NULL) {
      Symbol method_name = method->name;
      DispTabEntryP entry = new DispTabEntry(name, method_name);
      int *offset = parentnd->get_method_offset(method_name);
      if (offset == NULL) {
        disp_offset->addid(method_name, new int(_num_methods));
        disp_tab->addid(_num_methods, entry);
        _num_methods++;
      } else {
        // if found in ancestors
        disp_tab->addid(*offset, entry);
      }
    } // end if 
  } // end for

  if (cgen_debug)
    cout << "Class " << name << " methods: " << _num_methods << endl;
  code_disptab(str); // emit code

  // dfs step
  for (List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->build_disptab(str);
}

// Emit dispatch table for this CgenNode
void CgenNode::code_disptab(ostream &str) const {
  emit_disptable_ref(name, str);
  str << LABEL;
  for (int i = 0; i != _num_methods; ++i) {
    DispTabEntryP entry = probe_entry(i);
    str << WORD << entry->cls << METHOD_SEP << entry->method << endl;
  }
}

// Utility function for code_init().
// Emit code of attribute initialization in current class 
void CgenNode::code_init_attr(ostream& s) const {
  if (name == Object ||
      name == Int || 
      name == Bool ||
      name == Str) return;
  LOOP_LIST_NODE(i, features)
  {
    attr_class *attr = dynamic_cast<attr_class*>(features->nth(i));
    if (attr != NULL) {
      int attr_offset = get_attr_offset(attr->name);
      // simply skip no_expr, as attr already has default value
      if (dynamic_cast<no_expr_class*>(attr->init))
        continue;
      attr->init->code(s);
      emit_store(ACC, attr_offset, SELF, s);
    }
  }
}

// Emit *_init label definition
// On entry, 'so' object in ACC
// Returns value is also 'so'
void CgenNode::code_init(ostream& s) const {
  emit_init_ref(name, s);
  s << LABEL;
  emit_push(FP, s);           // ofp
  emit_push(SELF, s);         // oso
  emit_move(SELF, ACC, s);
  emit_push(RA, s);           // ra
  emit_addiu(FP, SP, WORD_SIZE, s); // get current fp
  if (name != Object)
    emit_jal_init_ref(parentnd->get_name(), s);
  code_init_attr(s);
  emit_move(ACC, SELF, s);    // returns 'so'
  emit_load(RA, 0, FP, s);    // restore ra
  emit_load(SELF, 1, FP, s);  // restore oso
  emit_load(FP, 2, FP, s);    // restore ofp
  emit_addiu(SP, SP, WORD_SIZE * PROLOG_SIZE, s);
  emit_return(s);
}

// Emit method definition for all methods
void CgenNode::code_method_def(ostream& s) {
  cur_cgnode = this;
  LOOP_LIST_NODE(i, features)
  {
    method_class *method = dynamic_cast<method_class*>(features->nth(i));
    if (method != NULL) method->code(s);
  }
}

// Map method name to offset O_f in dispatch table
int* CgenNode::get_method_offset(Symbol id) const {
  int *ret = NULL;
  ret = disp_offset->probe(id);
  if (ret != NULL) return ret;
  if (name == Object) return ret; // return NULL
  return get_parentnd()->get_method_offset(id);
}

int CgenNode::get_attr_offset(Symbol sym) const {
  return *attr_tab->lookup(sym);
}

// Retrive dispatch table entry from offset O_f
DispTabEntryP CgenNode::probe_entry(int offset) const {
  DispTabEntryP ret = NULL;
  ret = disp_tab->probe(offset);
  if (ret != NULL) return ret;
  return get_parentnd()->probe_entry(offset);
}

//******************************************************
// cgen function for Method definition 
//******************************************************
void method_class::code(ostream& s) {
  if (cgen_debug) cout << pad(2) << "coding " << name << endl;
  emit_method_def(cur_cgnode->get_name(), name, s);
  /**********************************/
  /************ prologue ************/
  /**********************************/
  cur_env = new EnvType();
  cur_env->enterscope();
  emit_push(FP, s);
  emit_push(SELF, s);
  emit_move(SELF, ACC, s); // 'so' in $so
  emit_push(RA, s);
  emit_addiu(FP, SP, WORD_SIZE, s); // get current fp
  fp_offset = PROLOG_SIZE + formals->len(); // $ra | $oso | $ofp | ..args.. 
  // calculate formal offset
  LOOP_LIST_NODE(i, formals)
  {
    formal_class *arg = dynamic_cast<formal_class*>(formals->nth(i));
    Symbol name = arg->name;
    cur_env->addid(name, new int(--fp_offset));
  }
  // calculate upper bound of sequential local variables
  /* int num_locals = expr->num_locals(); */ // TODO
  int num_locals = 0;
  emit_addiu(SP, SP, - WORD_SIZE * num_locals, s);
  fp_offset = -1;
  /**********************************/
  /********* prologue end ***********/
  /**********************************/

  expr->code(s);

  /**********************************/
  /************ epilogue ************/
  /**********************************/
  emit_load(RA, 0, FP, s);
  emit_load(SELF, 1, FP, s);
  emit_load(FP, 2, FP, s);
  //      ..locals[]..| $ra | | oso | ofp | ..args[].. 
  // <-- low --           Address               -- high -->             
  emit_addiu(SP, SP, 
      WORD_SIZE * (num_locals + formals->len() + PROLOG_SIZE), s);
  emit_return(s);
  cur_env->exitscope();
  /**********************************/
  /********** epilogue end **********/
  /**********************************/
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

static int label_index = 0;

static void code_dispatch(
    Expression expr, 
    Symbol type_name, 
    Symbol name, 
    Expressions actual, 
    ostream& s) 
{
  LOOP_LIST_NODE(i, actual)
  {
    actual->nth(i)->code(s);
    emit_push(ACC, s);
  }
  expr->code(s);  // callee's 'so' in ACC
  int nonzero_label = label_index++;
  emit_bne(ACC, ZERO, nonzero_label, s);
  emit_load_string(
      ACC, 
      stringtable.lookup_string(
        cur_cgnode->get_filename()->get_string()), 
      s);
  emit_load_imm(T1, cur_cgnode->get_line_number(), s); // TODO check
  emit_jal(DISPATCH_ABORT, s);
  emit_label_def(nonzero_label, s);

  CgenNodeP cgnode;
  if (type_name != NULL) {
    cgnode = cgen_classtable->lookup(type_name);        // static dispatch
    // actually not necessary
    int *offset = cgnode->get_method_offset(name);      
    emit_load_disptable(T1, type_name, s);
    // 'so' already in ACC
    emit_load(T1, *offset, T1, s); // load method
    emit_jalr(T1, s);
  } else {
    cgnode = cgen_classtable->lookup(expr->get_type()); // default dispatch
    int *offset = cgnode->get_method_offset(name);
    // 'so' already in ACC
    emit_load(T1, DISPTABLE_OFFSET, ACC, s);   // load dispTab
    emit_load(T1, *offset, T1, s); // load method
    emit_jalr(T1, s);
  }
}

static void code_arith(Expression e1, Expression e2, ArithType t, ostream& s) {
  e1->code(s);
  emit_fetch_int(ACC, ACC, s);  // push val field of e1
  emit_push(ACC, s);
  e2->code(s);
  emit_jal_method(Object, ::copy, s); // copy() is method of tree_node
  emit_fetch_int(T2, ACC, s);       // load val field of e2 
  emit_pop(T1, s);                  // load val field of e1
  switch(t) {
    case Plus:
      emit_add(T1, T1, T2, s);
      break;
    case Sub:
      emit_sub(T1, T1, T2, s);
      break;
    case Mul:
      emit_mul(T1, T1, T2, s);
      break;
    case Div:
      emit_div(T1, T1, T2, s); // TODO catch div 0?
      break;
    default:
      break;
  }
  emit_store_int(T1, ACC, s);
}

static void code_compare(
    Expression e1, Expression e2, CompareType t, ostream& s) {
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_move(T2, ACC, s);
  emit_pop(T1, s);
  emit_fetch_int(T1, T1, s); // load val_1 field
  emit_fetch_int(T2, T2, s); // load val_2 field
  int true_label = label_index++;
  switch(t) {
    case Less:
      emit_blt(T1, T2, true_label, s);
      break;
    case LessEqual:
      emit_bleq(T1, T2, true_label, s);
      break;
    default:
      break;
  }
  emit_load_falsebool(ACC, s);
  int end_label = label_index++;
  emit_branch(end_label, s);
  emit_label_def(true_label, s);
  emit_load_truebool(ACC, s);
  emit_label_def(end_label, s);
}

void assign_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code assign" << endl;
  // type check guarantees that 'self' will not be assigned
  expr->code(s);
  emit_push(ACC, s);

  // args and local vars
  int *offset = cur_env->lookup(name);
  if (offset != NULL) {
    if (cgen_debug) cout << pad(6) << "found arg/local" << endl;
    emit_addiu(ACC, FP, WORD_SIZE * (*offset), s);
  } else {
    // resort to attribute table
    if (cgen_debug) cout << pad(6) << "found attr" << endl;
    emit_addiu(ACC, SELF, WORD_SIZE *cur_cgnode->get_attr_offset(name), s);
  }

  emit_pop(T1, s);
  emit_store(T1, 0, ACC, s);
  emit_load(ACC, 0, ACC, s);
}

void static_dispatch_class::code(ostream &s) {
  // TODO if dispatch to SELFTYPE?
  if (cgen_debug) cout << pad(4) << "code static dispatch" << endl;
  code_dispatch(expr, type_name, name, actual, s);
}

void dispatch_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code dispatch" << endl;
  code_dispatch(expr, NULL, name, actual, s);
}

void cond_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code cond" << endl;
  pred->code(s);
  emit_fetch_int(ACC, ACC, s);
  int false_label = label_index++;
  emit_beqz(ACC, false_label, s);
  then_exp->code(s);                  // (default) true branch
  int end_label = label_index++; 
  emit_branch(end_label, s);
  emit_label_def(false_label, s);     // false branch
  else_exp->code(s);
  emit_label_def(end_label, s);       // end_if:
}

void loop_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code cond" << endl;
  int pred_label = label_index++;
  emit_label_def(pred_label, s);
  pred->code(s);
  emit_fetch_int(ACC, ACC, s);
  int end_label = label_index++;
  emit_beqz(ACC, end_label, s);
  body->code(s);
  emit_branch(pred_label, s);
  emit_label_def(end_label, s); // _end_loop:
  emit_load_imm(ACC, 0, s);
}

void typcase_class::code(ostream &s) {
}

void block_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code block" << endl;
  LOOP_LIST_NODE(i, body)
    body->nth(i)->code(s);
}

void let_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code let" << endl;
  if (cgen_debug) s << "\t# let begin\n";
  // cur_env is set in cgen of method definition (method_class:code())
  cur_env->enterscope();
  cur_env->addid(identifier, new int(fp_offset));

  if (dynamic_cast<no_expr_class*>(init)) {
    // let-no-init
    if (type_decl == Int) {
      IntEntryP int_default = inttable.lookup_string("0");
      emit_load_int(ACC, int_default, s);
    } else if (type_decl == Bool) {
      emit_load_falsebool(ACC, s);
    } else if (type_decl == Str) {
      StringEntryP str_default = stringtable.lookup_string("");
      emit_load_string(ACC, str_default, s);
    } else {
      emit_move(ACC, ZERO, s);
    }
  } else {
    init->code(s);
  }
  emit_store(ACC, fp_offset--, FP, s);
  body->code(s);
  /* fp_offset++; // ? TODO */
  cur_env->exitscope();
  if (cgen_debug) s << "\t# let end\n";
}

void plus_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code plus" << endl;
  code_arith(e1, e2, Plus, s);
}

void sub_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code sub" << endl;
  code_arith(e1, e2, Sub, s);
}

void mul_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code mul" << endl;
  code_arith(e1, e2, Mul, s);
}

void divide_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code divide" << endl;
  code_arith(e1, e2, Div, s);
}

void neg_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code neg" << endl;
  e1->code(s);
  emit_jal_method(Object, ::copy, s);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
}

void lt_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code less than" << endl;
  code_compare(e1, e2, Less, s);
}

void eq_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code equal" << endl;
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_move(T2, ACC, s);
  emit_pop(T1, s);
  int true_label = label_index++;
  emit_beq(T1, T2, true_label, s); 
  emit_load_truebool(ACC, s);
  emit_load_falsebool(A1, s);
  emit_jal(EQUALITY_TEST, s);         // if true, true const in a0, 
                                      // else false const in a0
  int end_label = label_index++;
  emit_branch(end_label, s);
  emit_label_def(true_label, s);
  emit_load_truebool(ACC, s);
  emit_label_def(end_label, s);
}

void leq_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code less-equal" << endl;
  code_compare(e1, e2, LessEqual, s);
}

void comp_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code comp(not)" << endl;
  e1->code(s);
  emit_jal_method(Object, ::copy, s);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_addiu(T1, T1, 1, s);
  emit_store_int(T1, ACC, s);
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code new" << endl;
  // the 'so' is NOT cgnode->name statically,
  // rather, it is the 'so' that passed in from caller dynamically
  if (type_name == SELF_TYPE) {
    // copy protObj
    emit_load(T1, TAG_OFFSET, SELF, s); // i = class_tag
    emit_load_imm(T2, 2, s);
    emit_mul(T1, T1, T2, s);
    emit_load_address(ACC, CLASSOBJTAB, s);
    emit_add(ACC, ACC, T1, s);  
    emit_push(ACC, s);          // caching (class_objTab+8*i)
    emit_load(ACC, 0, ACC, s);  // class_objTab[8*i]
    emit_jal_method(Object, ::copy, s);
    // init
    emit_pop(ACC, s);
    emit_load(ACC, 4, ACC, s); // class_obj[8*i+4] 
    emit_jalr(T1, s); // TODO? jalr or jal?
    return;
  }
  CgenNodeP cgnode = cgen_classtable->lookup(type_name);
  Symbol new_type = cgnode->get_name();
  emit_load_protobj_ref(ACC, new_type, s);
  emit_jal_method(Object, ::copy, s);
  emit_jal_init_ref(new_type, s);
}

void isvoid_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code isvoid" << endl;
  e1->code(s);
  int true_label = label_index++;
  emit_beqz(ACC, true_label, s);
  emit_load_falsebool(ACC, s);
  int end_label = label_index++;
  emit_branch(end_label, s);
  emit_label_def(true_label, s);
  emit_load_truebool(ACC, s);
  emit_label_def(end_label, s);
}

void no_expr_class::code(ostream &s) {
  cerr << "!!!!! Shout not call no_expr_class !!!!!!!" << endl;
} 

void object_class::code(ostream &s) {
  if (cgen_debug) cout << pad(4) << "code object" << endl;
  if (name == self) {
    emit_move(ACC, SELF, s);
    return;
  }
  // args and local vars
  int *offset = cur_env->lookup(name);
  if (offset != NULL) {
    if (cgen_debug) cout << pad(6) << "found arg/local" << endl;
    emit_load(ACC, *offset, FP, s);
    return;
  }
  // resort to attribute table
  if (cgen_debug) cout << pad(6) << "found attr" << endl;
  emit_load(ACC, cur_cgnode->get_attr_offset(name), SELF, s);
}


