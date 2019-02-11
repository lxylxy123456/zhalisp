#include "bool.h"

Bool::Bool(const char * const s) {
  if (s[1] != '\0' || (s[0] != 't' && s[0] != 'T'))
    throw std::invalid_argument("Incorrect type name");
}

Bool::~Bool() {
//  std::cout << "~Bool" << std::endl;
}

std::string Bool::str() const {
  return "T";
}

std::string Bool::repr() const {
  return "Bool<T>";
}

Type Bool::type() const {
  return bool_;
}

bool Bool::type(Type tid) const {
  return tid == sexp || tid == atom;
}

PTR<Bool> Bool::lisp_t(new Bool{"T"});

