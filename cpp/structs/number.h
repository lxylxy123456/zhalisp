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

#include "sexp.h"

#define DCCI dynamic_cast<const Integer&>
#define DCCR dynamic_cast<const Rational&>
#define DCCF dynamic_cast<const Float&>
#define DCCC dynamic_cast<const Complex&>

#define DPCN DPC<Number>
#define DPCI DPC<Integer>
#define DPCR DPC<Rational>
#define DPCF DPC<Float>
#define DPCC DPC<Complex>

#define PTRNI(X) PTR<Number>(new Integer(X))
#define PTRNR(X) PTR<Number>(new Rational(X))
#define PTRNF(X) PTR<Number>(new Float(X))
#define PTRNC(X, Y) PTR<Number>(new Complex(X, Y))

class Number: public Sexp {
 public:
  virtual ~Number();
  virtual std::string str() const = 0;
  virtual Type type() const;
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
  virtual bool is_0() const = 0;
};

class Integer;
class Rational;
class Float;
class Complex;

#endif
