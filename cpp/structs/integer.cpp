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

#include "integer.h"

#include "rational.h"
#include "float.h"
#include "complex.h"

Integer::Integer(const mpz_class& z): value(z) {}

Integer::~Integer() { /* std::cout << "~Integer" << std::endl; */ }

const mpz_class& Integer::get_value() const { return value; }

std::string Integer::str() const { return value.get_str(); }

Type Integer::type() const { return Type::integer; }

PTR<Integer> Integer::lisp_0(new Integer{0});

PTR<Integer> Integer::lisp_1(new Integer{1});

PTR<Integer> Integer::lisp_2(new Integer{2});

PTR<Number> Integer::operator+() const { return PTRNI(+value); }

PTR<Number> Integer::operator-() const { return PTRNI(-value); }

PTR<Number> Integer::operator+(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNI(value + DCCI(rhs).value);
    case Type::rational :
      return PTRNR(value + DCCR(rhs).value);
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

PTR<Number> Integer::operator-(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNI(value - DCCI(rhs).value);
    case Type::rational :
      return PTRNR(value - DCCR(rhs).value);
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

PTR<Number> Integer::operator*(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNI(value * DCCI(rhs).value);
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

PTR<Number> Integer::operator/(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return reduced_rational(value / mpq_class(DCCI(rhs).value));
    case Type::rational :
      return reduced_rational(value / DCCR(rhs).value);
    case Type::float_ :
      return PTRNF(value * DCCF(rhs).value);
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

bool Integer::operator==(const Number& rhs) const {
  return rhs.type() == Type::integer && value == DCCI(rhs).value;
}

bool Integer::operator<(const Number& rhs) const {
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

bool Integer::operator<=(const Number& rhs) const {
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

bool Integer::operator>(const Number& rhs) const {
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

bool Integer::operator>=(const Number& rhs) const {
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

PTR<Number> Integer::sqrt_() const {
  mpz_class int_result = ::sqrt(value);
  if (value == int_result * int_result)
    return PTRNI(int_result);
  else
    return PTRNF(::sqrt(mpf_class(value)));
}

bool Integer::is_0() const { return value == 0; }

