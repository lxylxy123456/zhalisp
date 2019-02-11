#include "nil.h"

Nil::Nil(): List(nullptr, nullptr) {}

Nil::~Nil() {
//  std::cout << "~Nil" << std::endl;
}

std::string Nil::str() const {
  return "NIL";
}

std::string Nil::repr() const {
  return "Nil<()>";
}

Type Nil::type() const {
  return null;
}

bool Nil::type(Type tid) const {
  return tid == sexp || tid == list || tid == null;
}

bool Nil::nil() const {
  return true;
}

const PTR<Sexp> Nil::car() const {
  return lisp_nil;
}

const PTR<List> Nil::cdr() const {
  return lisp_nil;
}

PTR<Nil> Nil::lisp_nil(new Nil{});

