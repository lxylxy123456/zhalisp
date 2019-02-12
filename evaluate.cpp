#include "evaluate.h"

#include <exception>

/*
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
*/

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
      case symbol :
        if (DPC<Symbol>(args->car())->get_value() == "+")
          return plus(args->cdr(), env);
        else if (DPC<Symbol>(args->car())->get_value() == "-")
          return minus(args->cdr(), env);
        else if (DPC<Symbol>(args->car())->get_value() == "*")
          return mul(args->cdr(), env);
        else if (DPC<Symbol>(args->car())->get_value() == "/")
          return div(args->cdr(), env);
        throw std::invalid_argument("To be implemented");
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

