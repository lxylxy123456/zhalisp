#ifndef LIST_H
#define LIST_H

#include "sexp.h"

class List: public Sexp {
 public:
  List(const PTR<Sexp>&, const PTR<List>&);
  virtual ~List();
  std::string str() const;
  std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type) const;
  virtual bool nil() const;
  virtual const PTR<Sexp> car() const;
  virtual const PTR<List> cdr() const;

 private:
  PTR<Sexp> l_car;
  PTR<List> l_cdr;
};

#endif
