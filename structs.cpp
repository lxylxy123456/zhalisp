#ifndef STRUCTS_CPP
#define STRUCTS_CPP

#include <gmpxx.h>

#include <cassert>
#include <memory>
#include <sstream>
#include <string>
#include <exception>

#define PTR std::shared_ptr

enum Type {
  sexp,
  dot,
  symbol,
  number,
  integer,  // Note: integers are rational
  rational,
  float_,
  complex,
  atom,
  bool_,
  list,
  null
};

const char * const type_desc[] = {
  "sexp",
  "dot",
  "symbol",
  "number",
  "integer",
  "rational",
  "float",
  "complex",
  "atom",
  "bool",
  "list",
  "null"
};

class Sexp;
class Dot;
class Symbol;
class Number;
class Integer;
class Rational;
class Float;
class Complex;
class Bool;
class List;
class Nil;
class SyntaxError;

class Sexp {
 public:
  virtual std::string str() const = 0;
  virtual std::string repr() const = 0;
  virtual Type type() const { return sexp; }
  virtual bool type(Type tid) const {
    return tid == sexp;
  }
  const char* strtype() { return type_desc[type()]; }
};

class Dot: public Sexp {
 public:
  // ~Dot() { std::cout << "~Dot" << std::endl; }
  virtual std::string str() const { return "."; }
  virtual std::string repr() const { return "DOT<.>"; }
  virtual Type type() const { return dot; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == dot;
  }
};

class Symbol: public Sexp {
 public:
  Symbol(std::string s): value(s) {}
  // ~Symbol() { std::cout << "~Symbol" << std::endl; }
  virtual std::string str() const { return value; }
  virtual std::string repr() const { return "SYMBOL<" + value + ">"; }
  virtual Type type() const { return symbol; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom || tid == symbol;
  }
  const std::string& get_value() { return value; }
  static PTR<Symbol> lisp_quote;
  static PTR<Symbol> lisp_function;

 private:
  std::string value;
};

class Number: public Sexp {
 public:
  virtual std::string str() const = 0;
  virtual std::string repr() const = 0;
  virtual Type type() const { return number; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == number;
  }
  virtual Number* operator+(const Sexp& rhs) const = 0;
};

class Integer: public Number {
 public:
  Integer(const mpz_class& z): value(z) {}
  // ~Integer() { std::cout << "~Integer" << std::endl; }
  virtual std::string str() const { return value.get_str(); }
  virtual std::string repr() const { return "INTEGER<" + str() + ">"; }
  virtual Type type() const { return integer; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom || tid == number || tid == integer ||
           tid == rational;
  }
  virtual Number* operator+(const Sexp& rhs) const;
  friend Rational;
  friend Float;
  friend Complex;

 private:
  mpz_class value;
};

class Rational: public Number {
 public:
  Rational(const mpq_class& q): value(q) { value.canonicalize(); }
  // ~Rational() { std::cout << "~Rational" << std::endl; }
  virtual std::string str() const { return value.get_str(); }
  virtual std::string repr() const { return "RATIONAL<" + str() + ">"; }
  virtual Type type() const { return rational; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom || tid == number || tid == rational;
  }
  virtual Number* operator+(const Sexp& rhs) const;
  friend Integer;
  friend Float;
  friend Complex;

 private:
  mpq_class value;
};

class Float: public Number {
 public:
  Float(const mpf_class& f): value(f) {}
  // ~Float() { std::cout << "~Float" << std::endl; }
  virtual std::string str() const {
    std::ostringstream os;
    os << value;
    return os.str();
  }
  virtual std::string repr() const { return "FLOAT<" + str() + ">"; }
  virtual Type type() const { return float_; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom || tid == number || tid == float_;
  }
  virtual Number* operator+(const Sexp& rhs) const;
  friend Integer;
  friend Rational;
  friend Complex;

 private:
  mpf_class value;
};

class Complex: public Number {
 public:
  Complex(const PTR<Number>& r,
          const PTR<Number>& i): real(r), imag(i) {}
  Complex(const PTR<Symbol>& c,
          const PTR<Number>& r,
          const PTR<Number>& i): Complex(r, i) {
            assert(c->get_value() == "C");
          }
  // ~Complex() { std::cout << "~Complex" << std::endl; }
  virtual std::string str() const {
    return "#C(" + real->str() + " " + imag->str() + ")";
  }
  virtual std::string repr() const { return "COMPLEX<" + str() + ">"; }
  virtual Type type() const { return complex; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom || tid == number || tid == complex;
  }
  virtual Number* operator+(const Sexp& rhs) const;
  friend Integer;
  friend Rational;
  friend Float;

 private:
  PTR<Number> real, imag;
};

class Bool: public Sexp {
 public:
  // ~Bool() { std::cout << "~Bool" << std::endl; }
  std::string str() const { return "T"; }
  std::string repr() const { return "Bool<T>"; }
  static PTR<Bool> lisp_t;
  virtual Type type() const { return bool_; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom;
  }
};

class List: public Sexp {
 public:
  List(const PTR<Sexp>& a,
       const PTR<List>& d): l_car(a), l_cdr(d) {}
  // ~List() { std::cout << "~List " << this << std::endl; }
  std::string str() const {
    std::string ans = "(" + this->car()->str();
    for (PTR<List> i = this->cdr(); !i->nil(); i = i->cdr())
      ans += " " + i->car()->str();
    return ans + ")";
  }
  std::string repr() const { return "List<" + str() + ">"; }
  virtual Type type() const { return list; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == list;
  }
  virtual bool nil() const { return false; }
  virtual const PTR<Sexp> car() const { return l_car; }
  virtual const PTR<List> cdr() const { return l_cdr; }

 private:
  PTR<Sexp> l_car;
  PTR<List> l_cdr;
};

class Nil: public List {
 public:
  Nil(): List(nullptr, nullptr) {}
  // ~Nil() { std::cout << "~Nil " << this << std::endl; }
  std::string str() const { return "NIL"; }
  std::string repr() const { return "Nil<()>"; }
  virtual Type type() const { return null; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == list || tid == null;
  }
  virtual bool nil() const { return true; }
  virtual const PTR<Sexp> car() const { return lisp_nil; }
  virtual const PTR<List> cdr() const { return lisp_nil; }
  static PTR<Nil> lisp_nil;
};

PTR<Symbol> Symbol::lisp_quote(new Symbol("QUOTE"));
PTR<Symbol> Symbol::lisp_function(new Symbol("FUNCTION"));
PTR<Bool> Bool::lisp_t(new Bool{});
PTR<Nil> Nil::lisp_nil(new Nil{});

class SyntaxError: public std::exception {
 public:
  SyntaxError(std::string d): desc(d) {}
  std::string desc;
};

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

#endif
