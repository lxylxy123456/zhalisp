#include "rational.h"

#include "integer.h"
#include "float.h"
#include "complex.h"

Rational::Rational(const mpq_class& q): value(q) {
  value.canonicalize();
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

Number* Rational::operator+(const Sexp& rhs) const {
  switch (rhs.type()) {
    case integer: {
      const Integer& r = dynamic_cast<const Integer&>(rhs);
      return new Rational(value + r.value);
    }
    case rational: {
      const Rational& r = dynamic_cast<const Rational&>(rhs);
      mpq_class result = value + r.value;
      if (result.get_den() == 1)
        return new Integer(result.get_num());
      else
        return new Rational(result);
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

