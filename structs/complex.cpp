#include "complex.h"

#include "integer.h"
#include "rational.h"
#include "float.h"

Complex::Complex(const PTR<Number>& r, const PTR<Number>& i): real(r),imag(i) {}

Complex::Complex(const PTR<Symbol>& c,
                 const PTR<Number>& r,
                 const PTR<Number>& i): Complex(r, i) {
                   assert(c->get_value() == "C");
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

Number* Complex::operator+(const Sexp& rhs) const {
  switch (rhs.type()) {
    case integer: {
      const Integer& r = dynamic_cast<const Integer&>(rhs);
      return new Complex(PTR<Number>(*real + r), imag);
    }
    case rational: {
      const Rational& r = dynamic_cast<const Rational&>(rhs);
      return new Complex(PTR<Number>(*real + r), imag);
    }
    case float_: {
      const Float& r = dynamic_cast<const Float&>(rhs);
      return new Complex(PTR<Number>(*real + r), imag);
    }
    case complex: {
      const Complex& r = dynamic_cast<const Complex&>(rhs);
      Number *re = *real + *r.real, *im = *imag + *r.imag;
      if (im->type(integer) && dynamic_cast<const Integer*>(im)->value == 0)
        return re;
      else
        return new Complex(PTR<Number>(re), PTR<Number>(im));
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

