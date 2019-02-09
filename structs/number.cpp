#include "number.h"

Type Number::type() const {
  return number;
}

bool Number::type(Type tid) const {
  return tid == sexp || tid == number;
}

