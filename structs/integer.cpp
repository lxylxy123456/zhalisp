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

PTR<Integer> Integer::lisp_0(new Integer{0});

PTR<Integer> Integer::lisp_1(new Integer{1});

PTR<Integer> Integer::lisp_2(new Integer{2});

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
      return PTRNC(*this + *r.real, r.imag);
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
      return PTRNC(*this - *r.real, r.imag);
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
      return reduced_complex(*this * *r.real, *this * *r.imag);
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
      PTR<Number> ri = *r.real * *r.imag;
      PTR<Number> denom = *ri * *ri;
      PTR<Number> rnum = *this * *r.real;
      PTR<Number> inum = *(-*this) * *r.imag;
      return reduced_complex(*rnum / *denom, *inum * *denom);
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

bool Integer::operator==(const Number& rhs) const {
  return rhs.type() == integer && value == DCCI(rhs).value;
}

bool Integer::operator<(const Number& rhs) const {
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

bool Integer::operator<=(const Number& rhs) const {
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

bool Integer::operator>(const Number& rhs) const {
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

bool Integer::operator>=(const Number& rhs) const {
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

PTR<Number> Integer::sqrt_() const {
  mpz_class int_result = ::sqrt(value);
  if (value == int_result * int_result)
    return PTRNI(int_result);
  else
    return PTRNF(::sqrt(mpf_class(value)));
}

