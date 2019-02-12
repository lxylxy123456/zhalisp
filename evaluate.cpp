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
  {"SETQ", setq},
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
  PTR<Number> ans(new Integer(0));
  for (auto i = args; !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPC<Number>(evaluate(i->car(), env));
    ans = PTR<Number>(ans->operator+(*rhs));
  }
  return ans;
}

PTR<Sexp> minus(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  else if (args->cdr()->nil())
    return DPC<Number>(args->car())->operator-();
  PTR<Number> ans = DPC<Number>(args->car());
  for (auto i = args->cdr(); !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPC<Number>(evaluate(i->car(), env));
    ans = PTR<Number>(ans->operator-(*rhs));
  }
  return ans;
}

PTR<Sexp> mul(PTR<List> args, ENV env) {
  PTR<Number> ans(new Integer(1));
  for (auto i = args; !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPC<Number>(evaluate(i->car(), env));
    ans = PTR<Number>(ans->operator*(*rhs));
  }
  return ans;
}

PTR<Sexp> div(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  else if (args->cdr()->nil())
    return PTR<Number>(new Integer(1))->operator/(*DPC<Number>(args->car()));
  PTR<Number> ans = DPC<Number>(args->car());
  for (auto i = args->cdr(); !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPC<Number>(evaluate(i->car(), env));
    ans = PTR<Number>(ans->operator/(*rhs));
  }
  return ans;
}

PTR<Sexp> one_plus(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPC<Number>(evaluate(args->car(), env));
  return opl->operator+(*PTR<Number>(new Integer(1)));
}

PTR<Sexp> one_minus(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Number> opl = DPC<Number>(evaluate(args->car(), env));
  return opl->operator-(*PTR<Number>(new Integer(1)));
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

// Unary Predicates

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
  case dot :
    throw std::invalid_argument("Unexpected input value");
  case symbol :
    return env->find_var(DPC<Symbol>(arg));
  case list : {
    std::shared_ptr<List> args = DPC<List>(arg);
    switch (args->car()->type()) {
      case symbol : {
        PTR<Sexp> (*f)(PTR<List>, ENV) = find_func(DPC<Symbol>(args->car()));
        return f(args->cdr(), env);
      }
      case list :
        throw std::invalid_argument("To be implemented (lambda)");
      default :
        throw std::invalid_argument("Not calling a function");
    }
  }
  case null :
  case integer :
  case rational :
  case float_ :
  case complex :
  case bool_ :
    return arg;
  default :   // sexp, number, atom
    throw std::invalid_argument("Unexpected type");
  }
}

