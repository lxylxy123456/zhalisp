#ifndef INTEGER_H
#define INTEGER_H

#include "number.h"

class Integer: public Number {
 public:
  Integer(const mpz_class&);
  virtual ~Integer();
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
  friend Rational;
  friend Float;
  friend Complex;
  friend PTR<Number> reduced_complex(PTR<Number>, PTR<Number>);

 private:
  mpz_class value;
};

#endif
