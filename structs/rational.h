#ifndef RATIONAL_H
#define RATIONAL_H

#include "number.h"

class Rational: public Number {
 public:
  Rational(const mpq_class&);
  virtual ~Rational();
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type) const;
  virtual Number* operator+(const Sexp&) const;
  virtual Number* operator-(const Sexp&) const;
  friend Integer;
  friend Float;
  friend Complex;

 private:
  mpq_class value;
};

#endif
