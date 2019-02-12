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

PTR<Number> Float::operator+() const {
  return PTRNF(+value);
}

PTR<Number> Float::operator-() const {
  return PTRNF(-value);
}

PTR<Number> Float::operator+(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNF(value + DCCI(rhs).value);
    case rational :
      return PTRNF(value + DCCR(rhs).value);
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

PTR<Number> Float::operator-(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNF(value - DCCI(rhs).value);
    case rational :
      return PTRNF(value - DCCR(rhs).value);
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

PTR<Number> Float::operator*(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNF(value * DCCI(rhs).value);
    case rational :
      return PTRNF(value * DCCR(rhs).value);
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

PTR<Number> Float::operator/(const Number& rhs) const {
  switch (rhs.type()) {
    case integer :
      return PTRNF(value / DCCI(rhs).value);
    case rational :
      return PTRNF(value / DCCR(rhs).value);
    case float_ :
      return PTRNF(value / DCCF(rhs).value);
    case complex: {
      const Complex& r = DCCC(rhs);
      PTR<Number> ri = PTR<Number>(*r.real * *r.imag);
      PTR<Number> denom = PTR<Number>(*ri * *ri);
      PTR<Number> rnum(*this * *r.real);
      PTR<Number> inum(*(-*this) * *r.imag);
      return reduced_complex(*rnum / *denom, PTR<Number>(*inum * *denom));
    }
    default:
      throw std::invalid_argument("Not number");
  }
}

bool Float::operator==(const Number& rhs) const {
  return rhs.type() == float_ && value == DCCF(rhs).value;
}

bool Float::operator<(const Number& rhs) const {
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

bool Float::operator<=(const Number& rhs) const {
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

bool Float::operator>(const Number& rhs) const {
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

bool Float::operator>=(const Number& rhs) const {
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

