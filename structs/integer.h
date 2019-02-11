#ifndef INTEGER_H
#define INTEGER_H

#include "number.h"

class Integer: public Number {
 public:
  Integer(const mpz_class& z);
  virtual ~Integer();
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type tid) const;
  virtual Number* operator+(const Sexp& rhs) const;
  friend Rational;
  friend Float;
  friend Complex;

 private:
  mpz_class value;
};

#endif
