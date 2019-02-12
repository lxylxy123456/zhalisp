#ifndef FLOAT_H
#define FLOAT_H

#include <sstream>

#include "number.h"

class Float: public Number {
 public:
  Float(const mpf_class&);
  virtual ~Float();
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
  friend Integer;
  friend Rational;
  friend Complex;

 private:
  mpf_class value;
};

#endif
