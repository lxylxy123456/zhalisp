#ifndef DOT_H
#define DOT_H

#include "sexp.h"

class Dot: public Sexp {
 public:
  virtual ~Dot();
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type) const;
};

#endif
