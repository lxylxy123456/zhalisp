#include "bool.h"

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

PTR<Bool> Bool::lisp_t(new Bool{});

