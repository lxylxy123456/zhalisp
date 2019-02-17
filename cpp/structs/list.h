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

#define PTRNL(X, Y) PTR<List>(new List{X, Y})

class List: public Sexp {
 public:
  List(const PTR<Sexp>&, const PTR<Sexp>&);
  virtual ~List();
  std::string str() const;
  virtual Type type() const;
  virtual const PTR<Sexp> car() const;
  virtual const PTR<List> cdr() const;
  virtual const PTR<Sexp> r_cdr() const;
  virtual PTR<Sexp>& rw_cdr();          // used for building list online, danger

 private:
  PTR<Sexp> l_car;
  PTR<Sexp> l_cdr;
};

#endif
