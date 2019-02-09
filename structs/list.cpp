#include "list.h"

List::List(const PTR<Sexp>& a, const PTR<List>& d): l_car(a), l_cdr(d) {}

std::string List::str() const {
  std::string ans = "(" + this->car()->str();
  for (PTR<List> i = this->cdr(); !i->nil(); i = i->cdr())
    ans += " " + i->car()->str();
  return ans + ")";
}

std::string List::repr() const {
  return "List<" + str() + ">";
}

Type List::type() const {
  return list;
}

bool List::type(Type tid) const {
  return tid == sexp || tid == list;
}

bool List::nil() const {
  return false;
}

const PTR<Sexp> List::car() const {
  return l_car;
}

const PTR<List> List::cdr() const {
  return l_cdr;
}

