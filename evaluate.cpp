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

/*
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
*/

// Helper functions

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
  {"SETQ", setq},
  {"ATOM", atom},
  {"LISTP", listp},
  {"NULL", null},
  {"NUMBERP", numberp},
  {"TYPEP", typep},
  {"SYMBOLP", symbolp},
  {"ZEROP", zerop},
  {"EVENP", evenp},
  {"ODDP", oddp},
};

// find_func takes sym and returns function from (PTR<List>, ENV) to PTR<Sexp>
PTR<Sexp> (*find_func(PTR<Symbol> sym))(PTR<List>, ENV) {
  PTR<Sexp>(*f)(PTR<List>, ENV) = fmap[sym->get_value()];
  if (!f)
    throw std::invalid_argument("Function not found");
  return f;
}

// Arithmetics

PTR<Sexp> plus(PTR<List> args, ENV env) {
  PTR<Number> ans = Integer::lisp_0;
  for (auto i = args; !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPC<Number>(evaluate(i->car(), env));
    ans = ans->operator+(*rhs);
  }
  return ans;
}

PTR<Sexp> minus(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  else if (args->cdr()->nil())
    return DPC<Number>(evaluate(args->car(), env))->operator-();
  PTR<Number> ans = DPC<Number>(evaluate(args->car(), env));
  for (auto i = args->cdr(); !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPC<Number>(evaluate(i->car(), env));
    ans = ans->operator-(*rhs);
  }
  return ans;
}

PTR<Sexp> mul(PTR<List> args, ENV env) {
  PTR<Number> ans(Integer::lisp_1);
  for (auto i = args; !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPC<Number>(evaluate(i->car(), env));
    ans = ans->operator*(*rhs);
  }
  return ans;
}

PTR<Sexp> div(PTR<List> args, ENV env) {
  if (args->nil()) {
    throw std::invalid_argument("Too few arguments");
  } else if (args->cdr()->nil()) {
    PTR<Number> evaluated = DPC<Number>(evaluate(args->car(), env));
    return PTR<Number>(Integer::lisp_1)->operator/(*evaluated);
  }
  PTR<Number> ans = DPC<Number>(evaluate(args->car(), env));
  for (auto i = args->cdr(); !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPC<Number>(evaluate(i->car(), env));
    ans = ans->operator/(*rhs);
  }
  return ans;
}

PTR<Sexp> one_plus(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPC<Number>(evaluate(args->car(), env));
  return opl->operator+(*PTR<Number>(Integer::lisp_1));
}

PTR<Sexp> one_minus(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPC<Number>(evaluate(args->car(), env));
  return opl->operator-(*PTR<Number>(Integer::lisp_1));
}

PTR<Sexp> eq_(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPC<Number>(evaluate(args->car(), env));
  PTR<Number> opr = DPC<Number>(evaluate(args->cdr()->car(), env));
  return BOOL(opl->operator==(*opr));
}

PTR<Sexp> lt(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPC<Number>(evaluate(args->car(), env));
  PTR<Number> opr = DPC<Number>(evaluate(args->cdr()->car(), env));
  return BOOL(opl->operator<(*opr));
}

PTR<Sexp> le(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPC<Number>(evaluate(args->car(), env));
  PTR<Number> opr = DPC<Number>(evaluate(args->cdr()->car(), env));
  return BOOL(opl->operator<=(*opr));
}

PTR<Sexp> gt(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPC<Number>(evaluate(args->car(), env));
  PTR<Number> opr = DPC<Number>(evaluate(args->cdr()->car(), env));
  return BOOL(opl->operator>(*opr));
}

PTR<Sexp> ge(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPC<Number>(evaluate(args->car(), env));
  PTR<Number> opr = DPC<Number>(evaluate(args->cdr()->car(), env));
  return BOOL(opl->operator>=(*opr));
}

PTR<Sexp> sqrt_(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> op = DPC<Number>(evaluate(args->car(), env));
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
  PTR<Symbol> type_name = DPC<Symbol>(args->cdr()->car());
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

// Logic

// List operations

// High-Order Functions

// Functions

// Variables

PTR<Sexp> setq(PTR<List> args, ENV env) {
  assert(args->cdr()->cdr()->nil());
  PTR<Symbol> k = DPC<Symbol>(args->car());
  PTR<Sexp> v = evaluate(args->cdr()->car(), env);
  env->set_var(k, v);
  return v;
}

// Conditions

// Iteration

// I/O

// Special functions

// Evaluate

PTR<Sexp> evaluate(PTR<Sexp> arg, ENV env) {
  switch (arg->type()) {
  case Type::dot :
    throw std::invalid_argument("Unexpected input value");
  case Type::symbol :
    return env->find_var(DPC<Symbol>(arg));
  case Type::list : {
    std::shared_ptr<List> args = DPC<List>(arg);
    switch (args->car()->type()) {
      case Type::symbol : {
        PTR<Sexp> (*f)(PTR<List>, ENV) = find_func(DPC<Symbol>(args->car()));
        return f(args->cdr(), env);
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

