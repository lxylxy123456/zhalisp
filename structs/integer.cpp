#include "integer.h"

#include "rational.h"
#include "float.h"
#include "complex.h"

Integer::Integer(const mpz_class& z): value(z) {}

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
    case integer: {
      const Integer& r = dynamic_cast<const Integer&>(rhs);
      return new Integer(value + r.value);
    }
    case rational: {
      const Rational& r = dynamic_cast<const Rational&>(rhs);
      return new Rational(value + r.value);
    }
    case float_: {
      const Float& r = dynamic_cast<const Float&>(rhs);
      return new Float(value + r.value);
    }
    case complex: {
      const Complex& r = dynamic_cast<const Complex&>(rhs);
      return new Complex(PTR<Number>(*this + *r.real), r.imag);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

