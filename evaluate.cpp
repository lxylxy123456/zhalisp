//
// zhalisp - A "zha" Clisp implementation
// Copyright (C) 2019  lxylxy123456
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

#include "evaluate.h"

#include <exception>
#include <map>

// Helper functions

bool is_eql(PTR<Sexp> a, PTR<Sexp> b) {
  Type ta = a->type();
  if (ta != b->type())
    return false;
  switch (ta) {
  case Type::integer :
    return *DPCI(a) == *DPCI(b);
  case Type::rational :
    return *DPCR(a) == *DPCR(b);
  case Type::float_ :
    return *DPCF(a) == *DPCF(b);
  case Type::complex :
    return *DPCC(a) == *DPCC(b);
  case Type::list :
    return a.get() == b.get();
  case Type::symbol :
    return DPCS(a)->get_value() == DPCS(b)->get_value();
  case Type::dot :
  case Type::null :
  case Type::boolean :
    return true;
  default :
    throw std::invalid_argument("Invalid type");
  }
}

bool is_equal(PTR<Sexp> a, PTR<Sexp> b) {
  Type ta = a->type();
  if (ta != b->type())
    return false;
  switch (ta) {
  case Type::integer :
    return *DPCI(a) == *DPCI(b);
  case Type::rational :
    return *DPCR(a) == *DPCR(b);
  case Type::float_ :
    return *DPCF(a) == *DPCF(b);
  case Type::complex :
    return *DPCC(a) == *DPCC(b);
  case Type::list : {
    PTR<List> la = DPCL(a), lb = DPCL(b);
    return is_equal(la->car(), lb->car()) && is_equal(la->r_cdr(), lb->r_cdr());
  }
  case Type::symbol :
    return DPCS(a)->get_value() == DPCS(b)->get_value();
  case Type::dot :
  case Type::null :
  case Type::boolean :
    return true;
  default :
    throw std::invalid_argument("Invalid type");
  }
}

// Arithmetics

PTR<Sexp> plus(PTR<List> args, ENV env) {
  PTR<Number> ans = Integer::lisp_0;
  for (auto i = args; i && !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPCN(evaluate(i->car(), env));
    ans = ans->operator+(*rhs);
  }
  return ans;
}

PTR<Sexp> minus(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  else if (args->cdr()->nil())
    return DPCN(evaluate(args->car(), env))->operator-();
  PTR<Number> ans = DPCN(evaluate(args->car(), env));
  for (auto i = args->cdr(); i && !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPCN(evaluate(i->car(), env));
    ans = ans->operator-(*rhs);
  }
  return ans;
}

PTR<Sexp> mul(PTR<List> args, ENV env) {
  PTR<Number> ans(Integer::lisp_1);
  for (auto i = args; i && !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPCN(evaluate(i->car(), env));
    ans = ans->operator*(*rhs);
  }
  return ans;
}

PTR<Sexp> div(PTR<List> args, ENV env) {
  if (args->nil()) {
    throw std::invalid_argument("Too few arguments");
  } else if (args->cdr()->nil()) {
    PTR<Number> evaluated = DPCN(evaluate(args->car(), env));
    return PTR<Number>(Integer::lisp_1)->operator/(*evaluated);
  }
  PTR<Number> ans = DPCN(evaluate(args->car(), env));
  for (auto i = args->cdr(); i && !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPCN(evaluate(i->car(), env));
    ans = ans->operator/(*rhs);
  }
  return ans;
}

PTR<Sexp> one_plus(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPCN(evaluate(args->car(), env));
  return opl->operator+(*PTR<Number>(Integer::lisp_1));
}

PTR<Sexp> one_minus(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPCN(evaluate(args->car(), env));
  return opl->operator-(*PTR<Number>(Integer::lisp_1));
}

PTR<Sexp> eq_(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPCN(evaluate(args->car(), env));
  PTR<Number> opr = DPCN(evaluate(args->cdr()->car(), env));
  return BOOL(opl->operator==(*opr));
}

PTR<Sexp> lt(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPCN(evaluate(args->car(), env));
  PTR<Number> opr = DPCN(evaluate(args->cdr()->car(), env));
  return BOOL(opl->operator<(*opr));
}

PTR<Sexp> le(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPCN(evaluate(args->car(), env));
  PTR<Number> opr = DPCN(evaluate(args->cdr()->car(), env));
  return BOOL(opl->operator<=(*opr));
}

PTR<Sexp> gt(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPCN(evaluate(args->car(), env));
  PTR<Number> opr = DPCN(evaluate(args->cdr()->car(), env));
  return BOOL(opl->operator>(*opr));
}

PTR<Sexp> ge(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPCN(evaluate(args->car(), env));
  PTR<Number> opr = DPCN(evaluate(args->cdr()->car(), env));
  return BOOL(opl->operator>=(*opr));
}

PTR<Sexp> sqrt_(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> op = DPCN(evaluate(args->car(), env));
  return op->sqrt_();
}

// Unary Predicates

PTR<Sexp> atom(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return BOOL(evaluate(args->car(), env)->has_type(Type::atom));
}

PTR<Sexp> listp(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return BOOL(evaluate(args->car(), env)->has_type(Type::list));
}

PTR<Sexp> null(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return BOOL(evaluate(args->car(), env)->has_type(Type::null));
}

PTR<Sexp> numberp(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return BOOL(evaluate(args->car(), env)->has_type(Type::number));
}

PTR<Sexp> typep(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments (not supporting 3rd arg)");
  PTR<Symbol> type_name = DPCS(args->cdr()->car());
  if (!type_name)
    throw std::invalid_argument("Invalid argument type");
  else if (type_name->get_value() == "ATOM")
    return BOOL(evaluate(args->car(), env)->has_type(Type::atom));
  else if (type_name->get_value() == "LIST")
    return BOOL(evaluate(args->car(), env)->has_type(Type::list));
  else if (type_name->get_value() == "NUMBER")
    return BOOL(evaluate(args->car(), env)->has_type(Type::number));
  else if (type_name->get_value() == "INTEGER")
    return BOOL(evaluate(args->car(), env)->has_type(Type::integer));
  else if (type_name->get_value() == "RATIONAL")
    return BOOL(evaluate(args->car(), env)->has_type(Type::rational));
  else if (type_name->get_value() == "FLOAT")
    return BOOL(evaluate(args->car(), env)->has_type(Type::float_));
  else if (type_name->get_value() == "COMPLEX")
    return BOOL(evaluate(args->car(), env)->has_type(Type::complex));
  else if (type_name->get_value() == "SYMBOL")
    return BOOL(evaluate(args->car(), env)->has_type(Type::symbol));
  else if (type_name->get_value() == "BOOLEAN")
    return BOOL(evaluate(args->car(), env)->has_type(Type::boolean));
  else if (type_name->get_value() == "NIL")
    return BOOL(false);
  else
    throw std::invalid_argument("Invalid type specifier or not implemented");
}

PTR<Sexp> symbolp(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return BOOL(evaluate(args->car(), env)->has_type(Type::symbol));
}

PTR<Sexp> zerop(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Integer> int1 = DPCI(evaluate(args->car(), env));
  if (!int1)
    throw std::invalid_argument("Invalid argument type");
  return BOOL(int1->get_value() == 0);
}

PTR<Sexp> evenp(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Integer> int1 = DPCI(evaluate(args->car(), env));
  if (!int1)
    throw std::invalid_argument("Invalid argument type");
  return BOOL(int1->get_value() % 2 == 0);
}

PTR<Sexp> oddp(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Integer> int1 = DPCI(evaluate(args->car(), env));
  if (!int1)
    throw std::invalid_argument("Invalid argument type");
  return BOOL(int1->get_value() % 2 == 1);
}

// Binary Predicates

PTR<Sexp> eq(PTR<List> args, ENV env) {
  // Same as eql
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Sexp> a = evaluate(args->car(), env);
  PTR<Sexp> b = evaluate(args->cdr()->car(), env);
  return BOOL(is_eql(a, b));
}

PTR<Sexp> eql(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Sexp> a = evaluate(args->car(), env);
  PTR<Sexp> b = evaluate(args->cdr()->car(), env);
  return BOOL(is_eql(a, b));
}

PTR<Sexp> equal(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Sexp> a = evaluate(args->car(), env);
  PTR<Sexp> b = evaluate(args->cdr()->car(), env);
  return BOOL(is_equal(a, b));
}

// Logic

PTR<Sexp> and_(PTR<List> args, ENV env) {
  if (!args || args->nil())
    return BOOL(true);
  PTR<Sexp> car;
  while ((car = evaluate(args->car(), env))->t() && (args = args->cdr()) &&
          args->t()) {}
  return car;
}

PTR<Sexp> or_(PTR<List> args, ENV env) {
  if (!args || args->nil())
    return BOOL(false);
  PTR<Sexp> car;
  while ((car = evaluate(args->car(), env))->nil() && (args = args->cdr()) &&
          args->t()) {}
  return car;
}

PTR<Sexp> not_(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return BOOL(evaluate(args->car(), env)->nil());
}

// List operations

PTR<Sexp> car(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return DPCL(evaluate(args->car(), env))->car();
}

PTR<Sexp> cdr(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return DPCL(evaluate(args->car(), env))->r_cdr();
}

// TODO: caordr

PTR<Sexp> cons(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Sexp> car = evaluate(args->car(), env);
  PTR<Sexp> cdr = evaluate(args->cdr()->car(), env);
  return PTRNL(car, cdr);
}

PTR<Sexp> list_(PTR<List> args, ENV env) {
  PTR<Sexp> ans = Nil::lisp_nil;
  PTR<Sexp>* next_ins = &ans;
  for (PTR<List> i = args; i && !i->nil(); i = i->cdr()) {
    *next_ins = PTRNL(evaluate(i->car(), env), Nil::lisp_nil);
    next_ins = &(DPCL(*next_ins))->rw_cdr();
  }
  return ans;
}

PTR<Sexp> member(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Sexp> x = evaluate(args->car(), env);
  PTR<List> l = DPCL(evaluate(args->cdr()->car(), env));
  while (!l->nil()) {
    if (is_eql(x, l->car()))
      return l;
    else
      l = l->cdr();
  }
  return l;
}

// High-Order Functions

PTR<Sexp> mapcar(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  // TODO: find func correctly. Now the syntax is "(mapcar + '(1 2) '(3 4))"
  // PTR<Sexp> func = evaluate(args->car(), env);
  auto func = find_func(DPCS(args->car()), env);
  std::vector<PTR<List>> arg_list;
  for (PTR<List> i = args->cdr(); i && !i->nil(); i = i->cdr()) {
    arg_list.push_back(DPC<List>(evaluate(i->car(), env)));
  }
  PTR<Sexp> ans = Nil::lisp_nil;
  PTR<Sexp>* next_ans = &ans;
  while (true) {
    PTR<Sexp> param = Nil::lisp_nil;
    PTR<Sexp>* next_param = &param;
    for (auto &i : arg_list) {
      if (i->nil())
        return ans;
      auto next = PTRNL(Symbol::lisp_quote, PTRNL(i->car(), Nil::lisp_nil));
      *next_param = PTRNL(next, Nil::lisp_nil);
      next_param = &(DPCL(*next_param))->rw_cdr();
      i = i->cdr();
    }
    auto ans_next = func(DPCL(param), env);
    *next_ans = PTRNL(ans_next, Nil::lisp_nil);
    next_ans = &(DPCL(*next_ans))->rw_cdr();
  }
}

// TODO: continue implementing from here

// Functions

// DEFUN

PTR<Sexp> defun(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  PTR<Symbol> f_name = DPCS(args->car());
  if (!f_name)
    throw std::invalid_argument("Invalid function name");
  PTR<List> f_args = DPCL(args->cdr()->car());
  if (!f_args)
    throw std::invalid_argument("Invalid argument list");
  for (PTR<List> i = f_args; !i->nil(); i = i->cdr())
    if (i->car()->type() != Type::symbol)
      throw std::invalid_argument("Formal argument not symbol");
  PTR<List> f_stmt = DPCL(args->cdr()->cdr());
  PTR<Func> func(new Func(f_name->get_value(), f_args, f_stmt, env));
  env->set_fun(f_name, func);
  return f_name;
}

// LAMBDA
// APPLY
// FUNCALL
// FUNCTION

PTR<Sexp> quote(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return args->car();
}

// Variables

PTR<Sexp> setq(PTR<List> args, ENV env) {
  int arg_count = 0;
  for (PTR<List> i = args; !i->nil(); i = i->cdr())
    arg_count++;
  if (arg_count % 2)
    throw std::invalid_argument("Odd number of arguments");
  PTR<Sexp> v = Nil::lisp_nil;
  for (PTR<List> i = args; !i->nil(); i = i->cdr()) {
    PTR<Symbol> k = DPCS(i->car());
    if (!k)
      throw std::invalid_argument("Not symbol");
    i = i->cdr();
    v = evaluate(i->car(), env);
    env->set_var(k, v);
  }
  return v;
}

// Conditions

// Iteration

// I/O

// Special functions

// Evaluate

std::map<const std::string, PTR<Sexp>(* const)(PTR<List>, ENV)> fmap = {
  {"+", plus},
  {"-", minus},
  {"*", mul},
  {"/", div},
  {"1+", one_plus},
  {"1-", one_minus},
  {"=", eq_},
  {"<", lt},
  {"<=", le},
  {">", gt},
  {">=", ge},
  {"SQRT", sqrt_},
  {"ATOM", atom},
  {"LISTP", listp},
  {"NULL", null},
  {"NUMBERP", numberp},
  {"TYPEP", typep},
  {"SYMBOLP", symbolp},
  {"ZEROP", zerop},
  {"EVENP", evenp},
  {"ODDP", oddp},
  {"EQ", eq},
  {"EQL", eql},
  {"EQUAL", equal},
  {"AND", and_},
  {"OR", or_},
  {"NOT", not_},
  {"CAR", car},
  {"CDR", cdr},
  {"CONS", cons},
  {"LIST", list_},
  {"MEMBER", member},
  {"MAPCAR", mapcar},
  
  {"DEFUN", defun},
  {"SETQ", setq},
  {"QUOTE", quote},
};

// find_func takes sym and returns function from (PTR<List>, ENV) to PTR<Sexp>
PTR<Sexp> (*find_func(PTR<Symbol> sym, ENV env))(PTR<List>, ENV) {
  PTR<Sexp>(*f)(PTR<List>, ENV) = fmap[sym->get_value()];
  if (!f)
    throw std::invalid_argument("Function not found");
  return f;
}

PTR<Sexp> evaluate(PTR<Sexp> arg, ENV env) {
  switch (arg->type()) {
  case Type::dot :
    throw std::invalid_argument("Unexpected input value");
  case Type::symbol :
    return env->find_var(DPCS(arg));
  case Type::list : {
    PTR<List> lst = DPC<List>(arg);
    switch (lst->car()->type()) {
    case Type::symbol : {
      PTR<Sexp> (*f)(PTR<List>, ENV) = find_func(DPCS(lst->car()), env);
      return f(lst->cdr(), env);
    }
    case Type::list :
      throw std::invalid_argument("To be implemented (lambda)");
    default :
      throw std::invalid_argument("Not calling a function");
    }
  }
  case Type::null :
  case Type::integer :
  case Type::rational :
  case Type::float_ :
  case Type::complex :
  case Type::boolean :
    return arg;
  default :   // sexp, number, atom
    throw std::invalid_argument("Unexpected type");
  }
}

