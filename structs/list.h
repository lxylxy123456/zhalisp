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

#ifndef LIST_H
#define LIST_H

#include "sexp.h"

class List: public Sexp {
 public:
  List(const PTR<Sexp>&, const PTR<List>&);
  virtual ~List();
  std::string str() const;
  std::string repr() const;
  virtual Type type() const;
  virtual bool type(Type) const;
  virtual bool nil() const;
  virtual const PTR<Sexp> car() const;
  virtual const PTR<List> cdr() const;

 private:
  PTR<Sexp> l_car;
  PTR<List> l_cdr;
};

#endif
