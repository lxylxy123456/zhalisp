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
  virtual Number* operator+() const;
  virtual Number* operator-() const;
  virtual Number* operator+(const Sexp&) const;
  virtual Number* operator-(const Sexp&) const;
  virtual Number* operator*(const Sexp&) const;
  virtual Number* operator/(const Sexp&) const;
  friend Rational;
  friend Float;
  friend Complex;
  friend Number* reduced_complex(Number*, PTR<Number>);

 private:
  mpz_class value;
};

#endif
