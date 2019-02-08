#ifndef STRUCTS_CPP
#define STRUCTS_CPP

#include <cassert>
#include <gmpxx.h>
#include <memory>
#include <sstream>
#include <string>
#include <exception>

#include <iostream> // 0/0

enum Type {
  sexp,
  dot,
  number,
  integer,  // Note: integers are rational
  rational,
  float_,
  complex,
  symbol,
  atom,
  list,
  null
};

class Sexp {
 public:
  virtual std::string str() const = 0;
  virtual std::string repr() const = 0;
  virtual std::string type() const { return "Sexp"; }
  virtual bool type(Type tid) const {
    return tid == sexp;
  }
};

class Dot: public Sexp {
 public:
  // ~Dot() { std::cout << "~Dot" << std::endl; }
  virtual std::string str() const { return "."; }
  virtual std::string repr() const { return "DOT<.>"; }
  virtual std::string type() const { return "Dot"; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == dot;
  }
};

class Number: public Sexp {
 public:
  virtual std::string str() const = 0;
  virtual std::string repr() const = 0;
  virtual std::string type() const { return "Number"; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == number;
  }
};

class Integer: public Number {
 public:
  Integer(const mpz_class& z): value(z) {}
  // ~Integer() { std::cout << "~Integer" << std::endl; }
  virtual std::string str() const { return value.get_str(); }
  virtual std::string repr() const { return "INTEGER<" + str() + ">"; }
  virtual std::string type() const { return "Integer"; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom || tid == number || tid == integer ||
           tid == rational;
  }

 private:
  mpz_class value;
};

class Rational: public Number {
 public:
  Rational(const mpq_class& q): value(q) {}
  // ~Rational() { std::cout << "~Rational" << std::endl; }
  virtual std::string str() const { return value.get_str(); }
  virtual std::string repr() const { return "RATIONAL<" + str() + ">"; }
  virtual std::string type() const { return "Rational"; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom || tid == number || tid == rational;
  }

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
  virtual std::string type() const { return "Float"; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom || tid == number || tid == float_;
  }

 private:
  mpf_class value;
};

/*
class Number: public Sexp {
 public:
  Number(int v): value(v) {}
  Number(std::string s) {
    std::istringstream ss(s);
    ss >> value;
    assert(ss.eof());
  }
  // ~Number() { std::cout << "~Number" << std::endl; }
  virtual std::string str() const { return std::to_string(value); }
  virtual std::string repr() const { return "NUMBER<" + str() + ">"; }
  virtual std::string type() const { return "Number"; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom || tid == number;
  }

 private:
  int value;  // TODO: allow other types of numbers
  // TODO: use GMP (#include <gmpxx.h>, -lgmp, -lgmpxx)
  // TODO: https://gmplib.org/manual/index.html
};
*/

class Symbol: public Sexp {
 public:
  Symbol(std::string s): value(s) {}
  // ~Symbol() { std::cout << "~Symbol" << std::endl; }
  virtual std::string str() const { return value; }
  virtual std::string repr() const { return "SYMBOL<" + value + ">"; }
  virtual std::string type() const { return "Symbol"; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom || tid == symbol;
  }
  const std::string& get_value() { return value; }
  static std::shared_ptr<Symbol> lisp_quote;
  static std::shared_ptr<Symbol> lisp_function;

 private:
  std::string value;
};

class Bool: public Sexp {
 public:
  // ~Bool() { std::cout << "~Bool" << std::endl; }
  std::string str() const { return "T"; }
  std::string repr() const { return "Bool<T>"; }
  static std::shared_ptr<Bool> lisp_t;
  virtual std::string type() const { return "Bool"; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == atom;
  }
};

class List: public Sexp {
 public:
  List(std::shared_ptr<Sexp> a, std::shared_ptr<List> d): l_car(a), l_cdr(d) {}
  // ~List() { std::cout << "~List " << this << std::endl; }
  std::string str() const {
    std::string ans = "(" + this->car()->str();
    for (std::shared_ptr<List> i = this->cdr(); !i->nil(); i = i->cdr())
      ans += " " + i->car()->str();
    return ans + ")";
  }
  std::string repr() const { return "List<" + str() + ">"; }
  virtual std::string type() const { return "List"; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == list;
  }
  virtual bool nil() const { return false; }
  virtual const std::shared_ptr<Sexp> car() const { return l_car; }
  virtual const std::shared_ptr<List> cdr() const { return l_cdr; }

 private:
  std::shared_ptr<Sexp> l_car;
  std::shared_ptr<List> l_cdr;
};

class Nil: public List {
 public:
  Nil(): List(nullptr, nullptr) {}
  // ~Nil() { std::cout << "~Nil " << this << std::endl; }
  std::string str() const { return "NIL"; }
  std::string repr() const { return "Nil<()>"; }
  virtual std::string type() const { return "Nil"; }
  virtual bool type(Type tid) const {
    return tid == sexp || tid == list || tid == null;
  }
  virtual bool nil() const { return true; }
  virtual const std::shared_ptr<Sexp> car() const { return lisp_nil; }
  virtual const std::shared_ptr<List> cdr() const { return lisp_nil; }
  static std::shared_ptr<Nil> lisp_nil;
};

std::shared_ptr<Symbol> Symbol::lisp_quote(new Symbol("QUOTE"));
std::shared_ptr<Symbol> Symbol::lisp_function(new Symbol("FUNCTION"));
std::shared_ptr<Bool> Bool::lisp_t(new Bool{});
std::shared_ptr<Nil> Nil::lisp_nil(new Nil{});

class SyntaxError: public std::exception {
 public:
  SyntaxError(std::string d): desc(d) {}
  std::string desc;
};

#endif

// #define TEST
#ifdef TEST
#include <iostream>
#include <typeinfo>

int main() {
	mpf_class F;
	F = "12344389.0";
  std::shared_ptr<Float> f = std::shared_ptr<Float>(new Float(F));
  std::cout << f->str() << std::endl;
  
  std::shared_ptr<List> b(new List(Bool::lisp_t, Nil::lisp_nil));
  std::shared_ptr<List> c(new List(std::shared_ptr<Sexp>(new Symbol{"10"}), b));
  std::shared_ptr<List> d(new List(std::shared_ptr<Sexp>(new Symbol{"19"}), c));

  std::cout << d->str() << std::endl;
}
#endif

