#ifndef BOOL_H
#define BOOL_H

#include "sexp.h"

class Bool: public Sexp {
 public:
  virtual ~Bool();
  std::string str() const;
  std::string repr() const;
  static PTR<Bool> lisp_t;
  virtual Type type() const;
  virtual bool type(Type tid) const;
};

#endif
