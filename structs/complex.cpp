#include "complex.h"

#include "integer.h"
#include "rational.h"
#include "float.h"

PTR<Number> reduced_complex(PTR<Number> re, PTR<Number> im) {
  if (im->type(integer) && DPCI(im)->value == 0)
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
  return complex;
}

bool Complex::type(Type tid) const {
  return tid == sexp || tid == atom || tid == number || tid == complex;
}

PTR<Number> Complex::operator+() const {
  return PTRNC(+*real, +*imag);
}

PTR<Number> Complex::operator-() const {
  return PTRNC(-*real, -*imag);
}

PTR<Number> Complex::operator+(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNC(*real + DCCI(rhs), imag);
    case rational :
      return PTRNC(*real + DCCR(rhs), imag);
    case float_ :
      return PTRNC(*real + DCCF(rhs), imag);
    case complex: {
      const Complex& r = DCCC(rhs);
      return reduced_complex(*real + *r.real, *imag + *r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Complex::operator-(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNC(*real - DCCI(rhs), imag);
    case rational :
      return PTRNC(*real - DCCR(rhs), imag);
    case float_ :
      return PTRNC(*real - DCCF(rhs), imag);
    case complex: {
      const Complex& r = DCCC(rhs);
      return reduced_complex(*real - *r.real, *imag - *r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Complex::operator*(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return reduced_complex(*real * DCCI(rhs), *imag * DCCI(rhs));
    case rational :
      return reduced_complex(*real * DCCR(rhs), *imag * DCCR(rhs));
    case float_ :
      return reduced_complex(*real * DCCF(rhs), *imag * DCCF(rhs));
    case complex: {
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
    case integer :
      return reduced_complex(*real / DCCI(rhs), *imag / DCCI(rhs));
    case rational :
      return reduced_complex(*real / DCCR(rhs), *imag / DCCR(rhs));
    case float_ :
      return reduced_complex(*real / DCCF(rhs), *imag / DCCF(rhs));
    case complex: {
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
  if (rhs.type() != complex)
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

