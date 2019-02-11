#include "evaluate.h"

#include <exception>

/*
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
*/

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
  case list :
    throw std::invalid_argument("To be implemented");
  case null :
    return arg;
  default :   // sexp, number, atom
    throw std::invalid_argument("Unexpected type");
  }
}

