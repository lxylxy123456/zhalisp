#include "float.h"

#include "integer.h"
#include "rational.h"
#include "complex.h"

Float::Float(const mpf_class& f): value(f) {}

Float::~Float() {
//  std::cout << "~Float" << std::endl;
}

std::string Float::str() const {
  std::ostringstream os;
  os << value;
  return os.str();
}

std::string Float::repr() const {
  return "FLOAT<" + str() + ">";
}

Type Float::type() const {
  return float_;
}

bool Float::type(Type tid) const {
  return tid == sexp || tid == atom || tid == number || tid == float_;
}

Number* Float::operator+(const Sexp& rhs) const {
  switch (rhs.type()) {
    case integer: {
      const Integer& r = dynamic_cast<const Integer&>(rhs);
      return new Float(value + r.value);
    }
    case rational: {
      const Rational& r = dynamic_cast<const Rational&>(rhs);
      return new Float(value + r.value);
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

