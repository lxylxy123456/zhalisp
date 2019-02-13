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

#ifndef COMPLEX_H
#define COMPLEX_H

#include <cassert>

#include "number.h"
#include "symbol.h"

PTR<Number> reduced_complex(PTR<Number> r, PTR<Number> i);

class Complex: public Number {
 public:
  Complex(const PTR<Number>&, const PTR<Number>&);
  virtual ~Complex();
  const PTR<Number>& get_real() const;
  const PTR<Number>& get_imag() const;
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
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
  friend Integer;
  friend Rational;
  friend Float;

 private:
  PTR<Number> real, imag;
};

#endif
