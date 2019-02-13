//
// zhalisp - A "zha" Clisp implementation
// Copyright (C) 2019  lxylxy123456
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

#ifndef NUMBER_H
#define NUMBER_H

#include <gmpxx.h>

#include "bool.h"
#include "nil.h"
#include "sexp.h"

#define DCCI dynamic_cast<const Integer&>
#define DCCR dynamic_cast<const Rational&>
#define DCCF dynamic_cast<const Float&>
#define DCCC dynamic_cast<const Complex&>

#define DPCI std::dynamic_pointer_cast<Integer>
#define DPCR std::dynamic_pointer_cast<Rational>
#define DPCF std::dynamic_pointer_cast<Float>
#define DPCC std::dynamic_pointer_cast<Complex>

#define PTRNI(X) PTR<Number>(new Integer(X))
#define PTRNR(X) PTR<Number>(new Rational(X))
#define PTRNF(X) PTR<Number>(new Float(X))
#define PTRNC(X, Y) PTR<Number>(new Complex(X, Y))

class Number: public Sexp {
 public:
  virtual ~Number();
  virtual std::string str() const = 0;
  virtual std::string repr() const = 0;
  virtual Type type() const;
  virtual bool type(Type) const;
  virtual PTR<Number> operator+() const = 0;
  virtual PTR<Number> operator-() const = 0;
  virtual PTR<Number> operator+(const Number&) const = 0;
  virtual PTR<Number> operator-(const Number&) const = 0;
  virtual PTR<Number> operator*(const Number&) const = 0;
  virtual PTR<Number> operator/(const Number&) const = 0;
  virtual bool operator==(const Number&) const = 0;
  virtual bool operator<(const Number&) const = 0;
  virtual bool operator<=(const Number&) const = 0;
  virtual bool operator>(const Number&) const = 0;
  virtual bool operator>=(const Number&) const = 0;
  virtual PTR<Number> sqrt_() const = 0;
};

class Integer;
class Rational;
class Float;
class Complex;

#endif
