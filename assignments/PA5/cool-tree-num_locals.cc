// *************************************************************
// This file contains function for calculating 
//  num_locals(),
// which is needed for allocating space for let/case expression
// *************************************************************

#ifndef COOL_TREE_NUM_LOCALS_H
#define COOL_TREE_NUM_LOCALS_H

#define LOOP_LIST_NODE(i, list) \
  for (int i = (list)->first(); (list)->more(i); i = (list)->next(i))

#define MAX(a, b) ((a) > (b)) ? (a) : (b)
#define MAX3(a, b, c) MAX(MAX((a), (b)), (c))

 #include "cool-tree.h"

int assign_class::num_locals() {
  return expr->num_locals();
}

static int num_locals(Expressions exprs) {
  int n = 0;
  LOOP_LIST_NODE(i, exprs)
  {
    n = MAX(n, exprs->nth(i)->num_locals());
  }
  return n;
}

int static_dispatch_class::num_locals() {
  int n1 = expr->num_locals();
  int n2 = ::num_locals(actual);
  return MAX(n1, n2);
}

int dispatch_class::num_locals() {
  int n1 = expr->num_locals();
  int n2 = ::num_locals(actual);
  return MAX(n1, n2);
}

int cond_class::num_locals() {
  int n1 = pred->num_locals();
  int n2 = then_exp->num_locals();
  int n3 = else_exp->num_locals();
  return MAX3(n1, n2, n3);
}

int loop_class::num_locals() {
  int n1 = pred->num_locals();
  int n2 = body->num_locals();
  return MAX(n1, n2);
}

int typcase_class::num_locals() {
  int n = expr->num_locals();
  LOOP_LIST_NODE(i, cases)
  {
    branch_class *branch = dynamic_cast<branch_class*>(cases->nth(i));
    if (branch)
      n = MAX(n,  1 + branch->expr->num_locals());
  }
  return n;
}

int block_class::num_locals() {
  return ::num_locals(body);
}

int let_class::num_locals() {
  int n_init = init->num_locals();
  int n_body = body->num_locals();
  return MAX(n_init, 1 + n_body);
}

int plus_class::num_locals() {
  int n1 = e1->num_locals();
  int n2 = e2->num_locals();
  return MAX(n1, n2);
}

int sub_class::num_locals() {
  int n1 = e1->num_locals();
  int n2 = e2->num_locals();
  return MAX(n1, n2);
}

int mul_class::num_locals() {
  int n1 = e1->num_locals();
  int n2 = e2->num_locals();
  return MAX(n1, n2);
}

int divide_class::num_locals() {
  int n1 = e1->num_locals();
  int n2 = e2->num_locals();
  return MAX(n1, n2);
}

int neg_class::num_locals() {
  return e1->num_locals();
}

int lt_class::num_locals() {
  int n1 = e1->num_locals();
  int n2 = e2->num_locals();
  return MAX(n1, n2);
}

int eq_class::num_locals() {
  int n1 = e1->num_locals();
  int n2 = e2->num_locals();
  return MAX(n1, n2);
}

int leq_class::num_locals() {
  int n1 = e1->num_locals();
  int n2 = e2->num_locals();
  return MAX(n1, n2);
}

int comp_class::num_locals() {
  return e1->num_locals();
}

int int_const_class::num_locals() {
  return 0;
}

int string_const_class::num_locals() {
  return 0;
}

int bool_const_class::num_locals() {
  return 0;
}

int new__class::num_locals() {
  return 0;
}

int isvoid_class::num_locals() {
  return e1->num_locals();
}

int no_expr_class::num_locals() {
  return 0;
}

int object_class::num_locals() {
  return 0;
}

#endif
