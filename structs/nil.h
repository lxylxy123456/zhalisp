#ifndef NIL_H
#define NIL_H

#include "list.h"

class Nil: public List {
 public:
  Nil();
  // ~Nil() { std::cout << "~Nil " << this << std::endl; }
  std::string str() const;
  std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type tid) const;
  virtual bool nil() const;
  virtual const PTR<Sexp> car() const;
  virtual const PTR<List> cdr() const;
  static PTR<Nil> lisp_nil;
};

#endif
