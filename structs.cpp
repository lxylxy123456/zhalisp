#ifndef STRUCTS_CPP
#define STRUCTS_CPP

#include <cassert>
#include <memory>
#include <sstream>
#include <string>

enum Type {
  sexp,
  dot,
  number,
  symbol,
  atom,
  list,
  null
};

class Sexp {
 public:
  virtual std::string str() const = 0;
  virtual std::string repr() const = 0;
  virtual bool type(Type tid) {
    return tid == sexp;
  }
};

class Dot: public Sexp {
 public:
  // ~Dot() { std::cout << "~Dot" << std::endl; }
  virtual std::string str() const { return "."; }
  virtual std::string repr() const { return "DOT<.>"; }
  virtual bool type(Type tid) {
    return tid == sexp || tid == dot;
  }
};

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
  virtual bool type(Type tid) {
    return tid == sexp || tid == atom || tid == number;
  }

 private:
  int value;  // TODO: allow other types of numbers
};

class Symbol: public Sexp {
 public:
  Symbol(std::string s): value(s) {}
  // ~Symbol() { std::cout << "~Symbol" << std::endl; }
  virtual std::string str() const { return value; }
  virtual std::string repr() const { return "SYMBOL<" + value + ">"; }
  virtual bool type(Type tid) {
    return tid == sexp || tid == atom || tid == symbol;
  }
  const std::string& get_value() { return value; }

 private:
  std::string value;
};

class Bool: public Sexp {
 public:
  // ~Bool() { std::cout << "~Bool" << std::endl; }
  std::string str() const { return "T"; }
  std::string repr() const { return "Bool<T>"; }
  static std::shared_ptr<Bool> lisp_t;
  virtual bool type(Type tid) {
    return tid == sexp || tid == atom;
  }
};

class List: public Sexp {
 public:
  List(std::shared_ptr<Sexp> a, std::shared_ptr<List> d): l_car(a), l_cdr(d) {}
  // ~List() { std::cout << "~List " << this << std::endl; }
  std::string str() const { return "0/0"; }
  std::string repr() const { return "0/0"; }
  virtual bool type(Type tid) {
    return tid == sexp || tid == list;
  }
  virtual bool nil() { return false; }
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
  std::string repr() const { return "List<()>"; }
  virtual bool type(Type tid) {
    return tid == sexp || tid == list || tid == null;
  }
  virtual bool nil() { return true; }
  virtual const std::shared_ptr<Sexp> car() const { return lisp_nil; }
  virtual const std::shared_ptr<List> cdr() const { return lisp_nil; }
  static std::shared_ptr<Nil> lisp_nil;
};

std::shared_ptr<Nil> Nil::lisp_nil(new Nil{});

std::shared_ptr<Bool> Bool::lisp_t(new Bool{});

#endif

#define TEST
#ifdef TEST
#include <iostream>
#include <typeinfo>

int main() {
  Number a("123");
  std::cout << a.str() << std::endl;
  std::cout << a.repr() << std::endl;
  std::shared_ptr<List> b(new List(Bool::lisp_t, Nil::lisp_nil));
  std::shared_ptr<List> c(new List(std::shared_ptr<Sexp>(new Number{10}), b));
  std::shared_ptr<List> d(new List(std::shared_ptr<Sexp>(new Number{19}), c));

  for (std::shared_ptr<List> i = d; i != Nil::lisp_nil; i = i->cdr()) {
    std::cout << i->car()->str() << ' ';
  }
  std::cout << std::endl;
}
#endif

