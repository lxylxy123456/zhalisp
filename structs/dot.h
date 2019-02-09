#ifndef DOT_H
#define DOT_H

#include "sexp.h"

class Dot: public Sexp {
 public:
  // ~Dot() { std::cout << "~Dot" << std::endl; }
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type tid) const;
};

#endif
