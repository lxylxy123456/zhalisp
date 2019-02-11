#ifndef NIL_H
#define NIL_H

#include "list.h"

class Nil: public List {
 public:
  Nil();
  virtual ~Nil();
  std::string str() const;
  std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type) const;
  virtual bool nil() const;
  virtual const PTR<Sexp> car() const;
  virtual const PTR<List> cdr() const;
  static PTR<Nil> lisp_nil;
};

#endif
