#include "sexp.h"

Type Sexp::type() const {
  return sexp;
}

bool Sexp::type(Type tid) const {
  return tid == sexp;
}

const char* Sexp::strtype() {
  return type_desc[type()];
}

