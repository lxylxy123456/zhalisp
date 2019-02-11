#ifndef COMPLEX_H
#define COMPLEX_H

#include <cassert>

#include "number.h"
#include "symbol.h"

class Complex: public Number {
 public:
  Complex(const PTR<Number>& r, const PTR<Number>& i);
  Complex(const PTR<Symbol>& c, const PTR<Number>& r, const PTR<Number>& i);
  virtual ~Complex();
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type tid) const;
  virtual Number* operator+(const Sexp& rhs) const;
  friend Integer;
  friend Rational;
  friend Float;

 private:
  PTR<Number> real, imag;
};

#endif
