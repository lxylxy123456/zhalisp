#ifndef SEXP_H
#define SEXP_H

#include <memory>
#include <string>

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

class Sexp {
 public:
  virtual std::string str() const = 0;
  virtual std::string repr() const = 0;
  virtual Type type() const;
  virtual bool type(Type tid) const;
  const char* strtype();
};

#endif
