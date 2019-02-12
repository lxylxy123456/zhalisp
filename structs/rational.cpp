#include "rational.h"

#include "integer.h"
#include "float.h"
#include "complex.h"

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

std::string Rational::str() const {
  return value.get_str();
}

std::string Rational::repr() const {
  return "RATIONAL<" + str() + ">";
}

Type Rational::type() const {
  return rational;
}

bool Rational::type(Type tid) const {
  return tid == sexp || tid == atom || tid == number || tid == rational;
}

PTR<Number> Rational::operator+() const {
  return PTRNR(+value);
}

PTR<Number> Rational::operator-() const {
  return PTRNR(-value);
}

PTR<Number> Rational::operator+(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNR(value + DCCI(rhs).value);
    case rational :
      return reduced_rational(value + DCCR(rhs).value);
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

PTR<Number> Rational::operator-(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNR(value - DCCI(rhs).value);
    case rational :
      return reduced_rational(value - DCCR(rhs).value);
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

PTR<Number> Rational::operator*(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return reduced_rational(value * DCCI(rhs).value);
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

PTR<Number> Rational::operator/(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return reduced_rational(value / DCCI(rhs).value);
    case rational :
      return reduced_rational(value / DCCR(rhs).value);
    case float_ :
      return PTRNF(value / DCCF(rhs).value);
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

bool Rational::operator==(const Number& rhs) const {
  return rhs.type() == rational && value == DCCR(rhs).value;
}

bool Rational::operator<(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return value < DCCI(rhs).value;
    case rational :
      return value < DCCR(rhs).value;
    case float_ :
      return value < DCCF(rhs).value;
    default:
      throw std::invalid_argument("Not a real number");
  }
}

bool Rational::operator<=(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return value <= DCCI(rhs).value;
    case rational :
      return value <= DCCR(rhs).value;
    case float_ :
      return value <= DCCF(rhs).value;
    default:
      throw std::invalid_argument("Not a real number");
  }
}

bool Rational::operator>(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return value > DCCI(rhs).value;
    case rational :
      return value > DCCR(rhs).value;
    case float_ :
      return value > DCCF(rhs).value;
    default:
      throw std::invalid_argument("Not a real number");
  }
}

bool Rational::operator>=(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return value >= DCCI(rhs).value;
    case rational :
      return value >= DCCR(rhs).value;
    case float_ :
      return value >= DCCF(rhs).value;
    default:
      throw std::invalid_argument("Not a real number");
  }
}

