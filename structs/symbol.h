#ifndef SYMBOL_H
#define SYMBOL_H

#include "sexp.h"

class Symbol: public Sexp {
 public:
  Symbol(std::string s);
  virtual ~Symbol();
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type tid) const;
  const std::string& get_value();
  static PTR<Symbol> lisp_quote;
  static PTR<Symbol> lisp_function;

 private:
  std::string value;
};

#endif
