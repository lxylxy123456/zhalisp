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

#ifndef NIL_H
#define NIL_H

#include "list.h"

class Nil: public List {
 public:
  Nil();
  virtual ~Nil();
  std::string str() const;
  virtual bool nil() const;
  virtual bool t() const;
  virtual Type type() const;
  virtual const PTR<Sexp> car() const;
  virtual const PTR<List> cdr() const;
  virtual const PTR<Sexp> r_cdr() const;
  virtual PTR<Sexp>& rw_cdr();          // disabled
  static PTR<Nil> lisp_nil;
};

#endif
