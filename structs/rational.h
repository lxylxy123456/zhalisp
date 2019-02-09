#ifndef RATIONAL_H
#define RATIONAL_H

#include "number.h"

class Rational: public Number {
 public:
  Rational(const mpq_class& q);
  // ~Rational() { std::cout << "~Rational" << std::endl; }
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type tid) const;
  virtual Number* operator+(const Sexp& rhs) const;
  friend Integer;
  friend Float;
  friend Complex;

 private:
  mpq_class value;
};

#endif
