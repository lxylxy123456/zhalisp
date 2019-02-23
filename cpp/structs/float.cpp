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

#include "float.h"

#include "integer.h"
#include "rational.h"
#include "complex.h"

Float::Float(const mpf_class& f): value(f) {}

Float::~Float() {
//  std::cout << "~Float" << std::endl;
}

const mpf_class& Float::get_value() const {
  return value;
}

std::string Float::str() const {
  std::ostringstream os;
  os << value;
  return os.str();
}

Type Float::type() const {
  return Type::float_;
}

PTR<Number> Float::operator+() const {
  return PTRNF(+value);
}

PTR<Number> Float::operator-() const {
  return PTRNF(-value);
}

PTR<Number> Float::operator+(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNF(value + DCCI(rhs).value);
    case Type::rational :
      return PTRNF(value + DCCR(rhs).value);
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

PTR<Number> Float::operator-(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNF(value - DCCI(rhs).value);
    case Type::rational :
      return PTRNF(value - DCCR(rhs).value);
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

PTR<Number> Float::operator*(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNF(value * DCCI(rhs).value);
    case Type::rational :
      return PTRNF(value * DCCR(rhs).value);
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

PTR<Number> Float::operator/(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNF(value / DCCI(rhs).value);
    case Type::rational :
      return PTRNF(value / DCCR(rhs).value);
    case Type::float_ :
      return PTRNF(value / DCCF(rhs).value);
    case Type::complex: {
      const Complex& r = DCCC(rhs);
      PTR<Number> ri = *r.real * *r.imag;
      PTR<Number> denom = *ri * *ri;
      PTR<Number> rnum(*this * *r.real);
      PTR<Number> inum(*(-*this) * *r.imag);
      return reduced_complex(*rnum / *denom, *inum * *denom);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

bool Float::operator==(const Number& rhs) const {
  return rhs.type() == Type::float_ && value == DCCF(rhs).value;
}

bool Float::operator<(const Number& rhs) const {
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

bool Float::operator<=(const Number& rhs) const {
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

bool Float::operator>(const Number& rhs) const {
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

bool Float::operator>=(const Number& rhs) const {
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

PTR<Number> Float::sqrt_() const {
  return PTRNF(::sqrt(value));
}

bool Float::is_0() const {
  return value == 0.0;
}

