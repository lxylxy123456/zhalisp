//
// zhalisp - A "zha" Clisp implementation
// Copyright (C) 2019  lxylxy123456
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

#ifndef SEXP_H
#define SEXP_H

#include <memory>
#include <string>

#ifdef CUSTOM_PTR
  #include "sptr.h"
  #define PTR sptr
  #define DPC sptr_cast
  #define SPTR_SWEEP(X) sptr_sweep(X)
#else
  #define PTR std::shared_ptr
  #define DPC std::dynamic_pointer_cast
  #define SPTR_SWEEP(X)
#endif

enum struct Type : int {
  sexp,     // not possible return value for type()
  symbol,
  number,   // not possible return value for type()
  integer,  // Note: integers are rational
  rational,
  float_,
  complex,
  atom,     // not possible return value for type()
  boolean,
  list,
  null,
  funcs,    // not possible return value for type()
  func,
  efunc,
  cadrfunc,
};

class Sexp {
 public:
  virtual ~Sexp();
  virtual std::string str() const = 0;
  virtual bool nil() const;
  virtual bool t() const;
  virtual Type type() const;
  bool has_type(Type) const;
  const char* strtype();
};

#endif
