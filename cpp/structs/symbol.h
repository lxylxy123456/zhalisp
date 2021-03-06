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

#ifndef SYMBOL_H
#define SYMBOL_H

#include "sexp.h"

#define DPCS DPC<Symbol>

class Symbol: public Sexp {
 public:
  Symbol(const char * const s);
  virtual ~Symbol();
  virtual std::string str() const;
  virtual Type type() const;
  const std::string& get_value();
  static PTR<Symbol> lisp_quote;
  static PTR<Symbol> lisp_function;

 private:
  std::string value;
};

#endif
