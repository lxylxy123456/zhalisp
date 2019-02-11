#ifndef BOOL_H
#define BOOL_H

#include "sexp.h"

class Bool: public Sexp {
 public:
  Bool(std::string);
  virtual ~Bool();
  std::string str() const;
  std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type) const;
  static PTR<Bool> lisp_t;
};

#endif
