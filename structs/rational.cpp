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

#include "rational.h"

#include "integer.h"
#include "float.h"
#include "complex.h"

Number* reduced_rational_ns(const mpq_class& q) {
  if (q.get_den() == 1)
    return new Integer{q.get_num()};
  else
    return new Rational{q};
}

PTR<Number> reduced_rational(const mpq_class& q) {
  if (q.get_den() == 1)
    return PTRNI(q.get_num());
  else
    return PTRNR(q);
}

Rational::Rational(const mpq_class& q): value(q) {
  value.canonicalize();
}

Rational::~Rational() {
//  std::cout << "~Rational" << std::endl;
}

const mpq_class& Rational::get_value() const {
  return value;
}

std::string Rational::str() const {
  return value.get_str();
}

std::string Rational::repr() const {
  return "RATIONAL<" + str() + ">";
}

Type Rational::type() const {
  return Type::rational;
}

PTR<Number> Rational::operator+() const {
  return PTRNR(+value);
}

PTR<Number> Rational::operator-() const {
  return PTRNR(-value);
}

PTR<Number> Rational::operator+(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNR(value + DCCI(rhs).value);
    case Type::rational :
      return reduced_rational(value + DCCR(rhs).value);
    case Type::float_ :
      return PTRNF(value + DCCF(rhs).value);
    case Type::complex: {
      const Complex& r = DCCC(rhs);
      return PTRNC(*this + *r.real, r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Rational::operator-(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNR(value - DCCI(rhs).value);
    case Type::rational :
      return reduced_rational(value - DCCR(rhs).value);
    case Type::float_ :
      return PTRNF(value - DCCF(rhs).value);
    case Type::complex: {
      const Complex& r = DCCC(rhs);
      return PTRNC(*this - *r.real, r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Rational::operator*(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return reduced_rational(value * DCCI(rhs).value);
    case Type::rational :
      return reduced_rational(value * DCCR(rhs).value);
    case Type::float_ :
      return PTRNF(value * DCCF(rhs).value);
    case Type::complex: {
      const Complex& r = DCCC(rhs);
      return reduced_complex(*this * *r.real, *this * *r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Rational::operator/(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return reduced_rational(value / DCCI(rhs).value);
    case Type::rational :
      return reduced_rational(value / DCCR(rhs).value);
    case Type::float_ :
      return PTRNF(value / DCCF(rhs).value);
    case Type::complex: {
      const Complex& r = DCCC(rhs);
      PTR<Number> ri = *r.real * *r.imag;
      PTR<Number> denom = *ri * *ri;
      PTR<Number> rnum = *this * *r.real;
      PTR<Number> inum = *(-*this) * *r.imag;
      return reduced_complex(*rnum / *denom, *inum * *denom);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

bool Rational::operator==(const Number& rhs) const {
  return rhs.type() == Type::rational && value == DCCR(rhs).value;
}

bool Rational::operator<(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return value < DCCI(rhs).value;
    case Type::rational :
      return value < DCCR(rhs).value;
    case Type::float_ :
      return value < DCCF(rhs).value;
    default:
      throw std::invalid_argument("Not a real number");
  }
}

bool Rational::operator<=(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return value <= DCCI(rhs).value;
    case Type::rational :
      return value <= DCCR(rhs).value;
    case Type::float_ :
      return value <= DCCF(rhs).value;
    default:
      throw std::invalid_argument("Not a real number");
  }
}

bool Rational::operator>(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return value > DCCI(rhs).value;
    case Type::rational :
      return value > DCCR(rhs).value;
    case Type::float_ :
      return value > DCCF(rhs).value;
    default:
      throw std::invalid_argument("Not a real number");
  }
}

bool Rational::operator>=(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return value >= DCCI(rhs).value;
    case Type::rational :
      return value >= DCCR(rhs).value;
    case Type::float_ :
      return value >= DCCF(rhs).value;
    default:
      throw std::invalid_argument("Not a real number");
  }
}

PTR<Number> Rational::sqrt_() const {
  mpq_class rat_result(::sqrt(value.get_num()), ::sqrt(value.get_den()));
  if (value == rat_result * rat_result)
    return PTRNR(rat_result);
  else
    return PTRNF(::sqrt(mpf_class(value)));
}

