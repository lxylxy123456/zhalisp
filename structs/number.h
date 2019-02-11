#ifndef NUMBER_H
#define NUMBER_H

#include <gmpxx.h>

#include "sexp.h"

class Number: public Sexp {
 public:
  virtual ~Number();
  virtual std::string str() const = 0;
  virtual std::string repr() const = 0;
  virtual Type type() const;
  virtual bool type(Type tid) const;
  virtual Number* operator+(const Sexp& rhs) const = 0;
};

class Integer;
class Rational;
class Float;
class Complex;

#endif
