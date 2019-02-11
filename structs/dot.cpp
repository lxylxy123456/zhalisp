#include "dot.h"

Dot::~Dot() {
//  std::cout << "~Dot" << std::endl;
}

std::string Dot::str() const {
  return ".";
}

std::string Dot::repr() const {
  return "DOT<.>";
}

Type Dot::type() const {
  return dot;
}

bool Dot::type(Type tid) const {
  return tid == sexp || tid == dot;
}
