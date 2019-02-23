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

#ifndef INTEGER_H
#define INTEGER_H

#include "number.h"

class Integer: public Number {
 public:
  Integer(const mpz_class&);
  virtual ~Integer();
  const mpz_class& get_value() const;
  virtual std::string str() const;
  virtual Type type() const;
  static PTR<Integer> lisp_0;
  static PTR<Integer> lisp_1;
  static PTR<Integer> lisp_2;
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
  virtual bool is_0() const;
  friend Rational;
  friend Float;
  friend Complex;

 private:
  mpz_class value;
};

#endif
