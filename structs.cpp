#ifndef STRUCTS_CPP
#define STRUCTS_CPP

#include <cassert>
#include <memory>
#include <sstream>
#include <string>

class Sexp {};

class Dot: public Sexp {
 public:
  std::string str() const { return "."; }
  std::string repr() const { return "DOT<.>"; }
};

class Number: public Sexp {
 public:
  Number(std::string s) {
    std::istringstream ss(s);
    ss >> value;
    assert(ss.eof());
  }
  std::string str() const { return std::to_string(value); }
  std::string repr() const { return "NUMBER<" + str() + ">"; }
 private:
  int value;  // TODO: allow other types of numbers
};

class Symbol: public Sexp {
 public:
  Symbol(std::string s): value(s) {}
  std::string str() const { return value; }
  std::string repr() const { return "SYMBOL<" + value + ">"; }
  const std::string& get_value() { return value; }
 private:
  std::string value;
};

class Bool: public Sexp {
 public:
  std::string str() const { return "T"; }
  std::string repr() const { return "Bool<T>"; }
};

class List: public Sexp {
 public:
  List(Sexp* a, List* d): l_car(a), l_cdr(d) {}
  List(std::shared_ptr<Sexp> a, std::shared_ptr<List> d): l_car(a), l_cdr(d) {}
    // TODO: Do I need it? ^
  std::string str() const { return "0/0"; }
  std::string repr() const { return "0/0"; }
  bool nil() { return false; }
  const Sexp& car() { return *l_car; }
  const List& cdr() { return *l_cdr; }
  std::shared_ptr<Sexp> l_car;
  std::shared_ptr<List> l_cdr;
};

class Nil: public List {
 public:
  Nil(): List(nullptr, nullptr) {}
  std::string str() const { return "NIL"; }
  std::string repr() const { return "List<()>"; }
  bool nil() { return true; }
  const Sexp& car() const { return *this; }
  const List& cdr() const { return *this; }
};

#endif

#define TEST
#ifdef TEST
#include <iostream>
#include <typeinfo>

int main() {
  Number a("123");
  std::cout << a.str() << std::endl;
  std::cout << a.repr() << std::endl;
  List b(new Bool{}, new Nil{});
  std::cout << b.str() << std::endl;
  std::cout << b.repr() << std::endl;
}
#endif

