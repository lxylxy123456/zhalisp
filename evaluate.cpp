#include "evaluate.h"

#include <exception>

/*
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
*/

PTR<Sexp> plus (PTR<List> args, T_ENV env) {
  std::shared_ptr<Number> op1 = DPC<Number>(args->car());
  std::shared_ptr<Number> op2 = DPC<Number>(args->cdr()->car());
  return PTR<Number>(op1->operator+(*op2));
}

PTR<Sexp> evaluate (PTR<Sexp> arg, T_ENV env) {
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
        assert(DPC<Symbol>(args->car())->get_value()=="+");
          // 0/0
        return plus(args->cdr(), env);
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

