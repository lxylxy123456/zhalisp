#include "evaluate.h"

#include <exception>

/*
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
*/

// find_func takes sym and returns function from (PTR<List>, T_ENV) to PTR<Sexp>
PTR<Sexp> (*find_func(PTR<Symbol> sym))(PTR<List>, T_ENV) {
  if (sym->get_value() == "+")
    return plus;
  else if (sym->get_value() == "-")
    return minus;
  else if (sym->get_value() == "*")
    return mul;
  else if (sym->get_value() == "/")
    return div;
  else
    throw std::invalid_argument("To be implemented");
}

PTR<Sexp> plus(PTR<List> args, T_ENV env) {
  PTR<Number> ans(new Integer(0));
  for (auto i = args; !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPC<Number>(evaluate(i->car(), env));
    ans = PTR<Number>(ans->operator+(*rhs));
  }
  return ans;
}

PTR<Sexp> minus(PTR<List> args, T_ENV env) {
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

PTR<Sexp> mul(PTR<List> args, T_ENV env) {
  PTR<Number> ans(new Integer(1));
  for (auto i = args; !i->nil(); i = i->cdr()) {
    PTR<Number> rhs = DPC<Number>(evaluate(i->car(), env));
    ans = PTR<Number>(ans->operator*(*rhs));
  }
  return ans;
}

PTR<Sexp> div(PTR<List> args, T_ENV env) {
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

PTR<Sexp> evaluate(PTR<Sexp> arg, T_ENV env) {
  switch (arg->type()) {
  case dot :
    throw std::invalid_argument("Unexpected input value");
  case symbol :
    throw std::invalid_argument("To be implemented");
  case integer :
    return arg;
  case rational :
    return arg;
  case float_ :
    return arg;
  case complex :
    return arg;
  case bool_ :
    return arg;
  case list : {
    std::shared_ptr<List> args = DPC<List>(arg);
    switch (args->car()->type()) {
      case symbol : {
        PTR<Sexp> (*f)(PTR<List>, T_ENV) = find_func(DPC<Symbol>(args->car()));
        return f(args->cdr(), env);
      }
      case list :
        throw std::invalid_argument("To be implemented (lambda)");
      default :
        throw std::invalid_argument("Not calling a function");
    }
  }
  case null :
    return arg;
  default :   // sexp, number, atom
    throw std::invalid_argument("Unexpected type");
  }
}

