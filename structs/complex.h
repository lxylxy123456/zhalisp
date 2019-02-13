#ifndef COMPLEX_H
#define COMPLEX_H

#include <cassert>

#include "number.h"
#include "symbol.h"

PTR<Number> reduced_complex(PTR<Number> r, PTR<Number> i);

class Complex: public Number {
 public:
  Complex(const PTR<Number>&, const PTR<Number>&);
  virtual ~Complex();
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type) const;
  virtual PTR<Number> operator+() const;
  virtual PTR<Number> operator-() const;
  virtual PTR<Number> operator+(const Number&) const;
  virtual PTR<Number> operator-(const Number&) const;
  virtual PTR<Number> operator*(const Number&) const;
  virtual PTR<Number> operator/(const Number&) const;
  virtual bool operator==(const Number&) const;
  virtual bool operator<(const Number&) const;
  virtual bool operator<=(const Number&) const;
  virtual bool operator>(const Number&) const;
  virtual bool operator>=(const Number&) const;
  virtual PTR<Number> sqrt_() const;
  friend Integer;
  friend Rational;
  friend Float;

 private:
  PTR<Number> real, imag;
};

#endif
