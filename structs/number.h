#ifndef NUMBER_H
#define NUMBER_H

#include <gmpxx.h>

#include "sexp.h"

#define DCCI dynamic_cast<const Integer&>
#define DCCR dynamic_cast<const Rational&>
#define DCCF dynamic_cast<const Float&>
#define DCCC dynamic_cast<const Complex&>

#define DPCI std::dynamic_pointer_cast<Integer>
#define DPCR std::dynamic_pointer_cast<Rational>
#define DPCF std::dynamic_pointer_cast<Float>
#define DPCC std::dynamic_pointer_cast<Complex>

class Number: public Sexp {
 public:
  virtual ~Number();
  virtual std::string str() const = 0;
  virtual std::string repr() const = 0;
  virtual Type type() const;
  virtual bool type(Type) const;
  virtual Number* operator+(const Sexp&) const = 0;
  virtual Number* operator-(const Sexp&) const = 0;
};

class Integer;
class Rational;
class Float;
class Complex;

#endif
