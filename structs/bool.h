#ifndef BOOL_H
#define BOOL_H

#include "sexp.h"

class Bool: public Sexp {
 public:
  // ~Bool() { std::cout << "~Bool" << std::endl; }
  std::string str() const;
  std::string repr() const;
  static PTR<Bool> lisp_t;
  virtual Type type() const;
  virtual bool type(Type tid) const;
};

#endif
