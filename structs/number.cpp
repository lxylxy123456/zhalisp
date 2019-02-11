#include "number.h"

Number::~Number() {
//  std::cout << "~Number" << std::endl;
}

Type Number::type() const {
  return number;
}

bool Number::type(Type tid) const {
  return tid == sexp || tid == number;
}

