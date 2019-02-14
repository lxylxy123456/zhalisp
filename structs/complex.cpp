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

#include "complex.h"

#include "integer.h"
#include "rational.h"
#include "float.h"

Number* reduced_complex(Number* re, PTR<Number> im) {
  if (im->has_type(Type::integer) && DPCI(im)->get_value() == 0)
    return re;
  else
    return new Complex{PTR<Number>(re), im};
}

PTR<Number> reduced_complex(PTR<Number> re, PTR<Number> im) {
  if (im->has_type(Type::integer) && DPCI(im)->get_value() == 0)
    return re;
  else
    return PTRNC(re, im);
}

Complex::Complex(const PTR<Number>&r, const PTR<Number>&i): real(r), imag(i) {}

Complex::~Complex() {
//  std::cout << "~Complex" << std::endl;
}

std::string Complex::str() const {
  return "#C(" + real->str() + " " + imag->str() + ")";
}

std::string Complex::repr() const {
  return "COMPLEX<" + str() + ">";
}

Type Complex::type() const {
  return Type::complex;
}

const PTR<Number>& Complex::get_real() const {
  return real;
}

const PTR<Number>& Complex::get_imag() const {
  return imag;
}

PTR<Number> Complex::operator+() const {
  return PTRNC(+*real, +*imag);
}

PTR<Number> Complex::operator-() const {
  return PTRNC(-*real, -*imag);
}

PTR<Number> Complex::operator+(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNC(*real + DCCI(rhs), imag);
    case Type::rational :
      return PTRNC(*real + DCCR(rhs), imag);
    case Type::float_ :
      return PTRNC(*real + DCCF(rhs), imag);
    case Type::complex: {
      const Complex& r = DCCC(rhs);
      return reduced_complex(*real + *r.real, *imag + *r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Complex::operator-(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return PTRNC(*real - DCCI(rhs), imag);
    case Type::rational :
      return PTRNC(*real - DCCR(rhs), imag);
    case Type::float_ :
      return PTRNC(*real - DCCF(rhs), imag);
    case Type::complex: {
      const Complex& r = DCCC(rhs);
      return reduced_complex(*real - *r.real, *imag - *r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Complex::operator*(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return reduced_complex(*real * DCCI(rhs), *imag * DCCI(rhs));
    case Type::rational :
      return reduced_complex(*real * DCCR(rhs), *imag * DCCR(rhs));
    case Type::float_ :
      return reduced_complex(*real * DCCF(rhs), *imag * DCCF(rhs));
    case Type::complex: {
      const Complex& r = DCCC(rhs);
      PTR<Number> r1r2 = *real * *r.real;
      PTR<Number> i1i2 = *imag * *r.imag;
      PTR<Number> i1r2 = *imag * *r.real;
      PTR<Number> r1i2 = *real * *r.imag;
      return reduced_complex(*r1r2 - *i1i2, *i1r2 + *r1i2);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Complex::operator/(const Number& rhs) const {
  switch (rhs.type()) {
    case Type::integer :
      return reduced_complex(*real / DCCI(rhs), *imag / DCCI(rhs));
    case Type::rational :
      return reduced_complex(*real / DCCR(rhs), *imag / DCCR(rhs));
    case Type::float_ :
      return reduced_complex(*real / DCCF(rhs), *imag / DCCF(rhs));
    case Type::complex: {
      const Complex& r = DCCC(rhs);
      PTR<Number> r2i2 = *r.real * *r.imag;
      PTR<Number> r2i22 = *r2i2 * *r2i2;
      PTR<Number> r1r2 = *real * *r.real;
      PTR<Number> i1i2 = *imag * *r.imag;
      PTR<Number> i1r2 = *imag * *r.real;
      PTR<Number> r1i2 = *real * *r.imag;
      return reduced_complex(*r1r2 + *i1i2, *i1r2 - *r1i2);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

bool Complex::operator==(const Number& rhs) const {
  if (rhs.type() != Type::complex)
    return false;
  const Complex& r = DCCC(rhs);
  return real == r.real && imag == r.imag;
}

bool Complex::operator<(const Number& rhs) const {
  throw std::invalid_argument("Not a real number");
}

bool Complex::operator<=(const Number& rhs) const {
  throw std::invalid_argument("Not a real number");
}

bool Complex::operator>(const Number& rhs) const {
  throw std::invalid_argument("Not a real number");
}

bool Complex::operator>=(const Number& rhs) const {
  throw std::invalid_argument("Not a real number");
}

PTR<Number> Complex::sqrt_() const {
  // tex(solve([a = c**2 - d**2, b = 2 * c * d], [c, d])[2]);
  PTR<Number> sa2b2 = (*(*real * *real) + *(*imag * *imag))->sqrt_();
  PTR<Number> c = (*(*sa2b2 + *real) / *Integer::lisp_2)->sqrt_();
  return reduced_complex(c, *c * *(*(*sa2b2 - *real) / *imag));
}

