#ifndef COMPLEX_H
#define COMPLEX_H

#include <cassert>

#include "number.h"
#include "symbol.h"

class Complex: public Number {
 public:
  Complex(const PTR<Number>&, const PTR<Number>&);
  Complex(const PTR<Symbol>&, const PTR<Number>&, const PTR<Number>&);
  virtual ~Complex();
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type) const;
  virtual Number* operator+(const Sexp&) const;
  friend Integer;
  friend Rational;
  friend Float;

 private:
  PTR<Number> real, imag;
};

#endif
