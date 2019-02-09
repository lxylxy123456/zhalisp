#include "symbol.h"

Symbol::Symbol(std::string s): value(s) {}

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

