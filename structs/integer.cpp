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

Number* Integer::operator+(const Sexp& rhs) const {
  switch (rhs.type()) {
    case integer :
      return new Integer(value + DCCI(rhs).value);
    case rational :
      return new Rational(value + DCCR(rhs).value);
    case float_ :
      return new Float(value + DCCF(rhs).value);
    case complex: {
      const Complex& r = DCCC(rhs);
      return new Complex(PTR<Number>(*this + *r.real), r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

Number* Integer::operator-(const Sexp& rhs) const {
  switch (rhs.type()) {
    case integer :
      return new Integer(value - DCCI(rhs).value);
    case rational :
      return new Rational(value - DCCR(rhs).value);
    case float_ :
      return new Float(value - DCCF(rhs).value);
    case complex: {
      const Complex& r = DCCC(rhs);
      return new Complex(PTR<Number>(*this - *r.real), r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

