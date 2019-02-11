#include "symbol.h"

#include <algorithm>
#include <cctype>

Symbol::Symbol(const char * const s): value(s) {
  std::transform(value.begin(), value.end(), value.begin(), toupper);
}

Symbol::~Symbol() {
//  std::cout << "~Symbol" << std::endl;
}

std::string Symbol::str() const {
  return value;
}

std::string Symbol::repr() const {
  return "SYMBOL<" + value + ">";
}

Type Symbol::type() const {
  return symbol;
}

bool Symbol::type(Type tid) const {
  return tid == sexp || tid == atom || tid == symbol;
}

const std::string& Symbol::get_value() {
  return value;
}

PTR<Symbol> Symbol::lisp_quote(new Symbol("QUOTE"));
PTR<Symbol> Symbol::lisp_function(new Symbol("FUNCTION"));

