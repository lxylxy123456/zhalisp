#ifndef SEXP_H
#define SEXP_H

#include <memory>
#include <string>

#define PTR std::shared_ptr

enum Type {
  sexp,			// not possible return value for type()
  dot,
  symbol,
  number,		// not possible return value for type()
  integer,  // Note: integers are rational
  rational,
  float_,
  complex,
  atom,			// not possible return value for type()
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
  virtual ~Sexp();
  virtual std::string str() const = 0;
  virtual std::string repr() const = 0;
  virtual Type type() const;
  virtual bool type(Type) const;
  const char* strtype();
};

#endif
