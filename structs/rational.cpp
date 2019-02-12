#include "rational.h"

#include "integer.h"
#include "float.h"
#include "complex.h"

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

Number* Rational::operator+(const Sexp& rhs) const {
  switch (rhs.type()) {
    case integer :
      return new Rational(value + DCCI(rhs).value);
    case rational: {
      mpq_class result = value + DCCR(rhs).value;
      if (result.get_den() == 1)
        return new Integer(result.get_num());
      else
        return new Rational(result);
    }
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

Number* Rational::operator-(const Sexp& rhs) const {
  switch (rhs.type()) {
    case integer :
      return new Rational(value - DCCI(rhs).value);
    case rational: {
      mpq_class result = value - DCCR(rhs).value;
      if (result.get_den() == 1)
        return new Integer(result.get_num());
      else
        return new Rational(result);
    }
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

