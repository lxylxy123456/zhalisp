#include "integer.h"

#include "rational.h"
#include "float.h"
#include "complex.h"

Integer::Integer(const mpz_class& z): value(z) {}

Integer::~Integer() {
//  std::cout << "~Integer" << std::endl;
}

std::string Integer::str() const {
  return value.get_str();
}

std::string Integer::repr() const {
  return "INTEGER<" + str() + ">";
}

Type Integer::type() const {
  return integer;
}

bool Integer::type(Type tid) const {
  return tid == sexp || tid == atom || tid == number || tid == integer ||
         tid == rational;
}

PTR<Number> Integer::operator+() const {
  return PTRNI(+value);
}

PTR<Number> Integer::operator-() const {
  return PTRNI(-value);
}

PTR<Number> Integer::operator+(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNI(value + DCCI(rhs).value);
    case rational :
      return PTRNR(value + DCCR(rhs).value);
    case float_ :
      return PTRNF(value + DCCF(rhs).value);
    case complex: {
      const Complex& r = DCCC(rhs);
      return PTRNC(PTR<Number>(*this + *r.real), r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Integer::operator-(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNI(value - DCCI(rhs).value);
    case rational :
      return PTRNR(value - DCCR(rhs).value);
    case float_ :
      return PTRNF(value - DCCF(rhs).value);
    case complex: {
      const Complex& r = DCCC(rhs);
      return PTRNC(PTR<Number>(*this - *r.real), r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Integer::operator*(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNI(value * DCCI(rhs).value);
    case rational :
      return reduced_rational(value * DCCR(rhs).value);
    case float_ :
      return PTRNF(value * DCCF(rhs).value);
    case complex: {
      const Complex& r = DCCC(rhs);
      return reduced_complex(*this * *r.real, PTR<Number>(*this * *r.imag));
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

PTR<Number> Integer::operator/(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return reduced_rational(value / mpq_class(DCCI(rhs).value));
    case rational :
      return reduced_rational(value / DCCR(rhs).value);
    case float_ :
      return PTRNF(value * DCCF(rhs).value);
    case complex: {
      const Complex& r = DCCC(rhs);
      PTR<Number> ri = PTR<Number>(*r.real * *r.imag);
      PTR<Number> denom = PTR<Number>(*ri * *ri);
      PTR<Number> rnum = PTR<Number>(*this * *r.real);
      PTR<Number> inum = PTR<Number>(*(-*this) * *r.imag);
      return reduced_complex(*rnum / *denom, PTR<Number>(*inum * *denom));
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

