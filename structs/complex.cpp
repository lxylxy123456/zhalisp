#include "complex.h"

#include "integer.h"
#include "rational.h"
#include "float.h"

Complex::Complex(const PTR<Number>&r, const PTR<Number>&i): real(r), imag(i) {}

Complex::Complex(const PTR<Symbol>& c,
                 const PTR<Number>& r,
                 const PTR<Number>& i): Complex(r, i) {
                   assert(c->get_value() == "C");
                 }

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

Number* Complex::operator+(const Sexp& rhs) const {
  switch (rhs.type()) {
    case integer :
      return new Complex(PTR<Number>(*real + DCCI(rhs)), imag);
    case rational :
      return new Complex(PTR<Number>(*real + DCCR(rhs)), imag);
    case float_ :
      return new Complex(PTR<Number>(*real + DCCF(rhs)), imag);
    case complex: {
      const Complex& r = DCCC(rhs);
      Number *re = *real + *r.real;
      PTR<Number> im(*imag + *r.imag);
      if (im->type(integer) && DPCI(im)->value == 0)
        return re;
      else
        return new Complex(PTR<Number>(re), im);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

Number* Complex::operator-(const Sexp& rhs) const {
  switch (rhs.type()) {
    case integer :
      return new Complex(PTR<Number>(*real - DCCI(rhs)), imag);
    case rational :
      return new Complex(PTR<Number>(*real - DCCR(rhs)), imag);
    case float_ :
      return new Complex(PTR<Number>(*real - DCCF(rhs)), imag);
    case complex: {
      const Complex& r = DCCC(rhs);
      Number *re = *real - *r.real;
      PTR<Number> im(*imag - *r.imag);
      if (im->type(integer) && DPCI(im)->value == 0)
        return re;
      else
        return new Complex(PTR<Number>(re), im);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

