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

#include "list.h"

List::List(const PTR<Sexp>& a, const PTR<List>& d): l_car(a), l_cdr(d) {}

List::~List() {
//  std::cout << "~List" << std::endl;
}

std::string List::str() const {
  std::string ans = "(" + this->car()->str();
  for (PTR<List> i = this->cdr(); !i->nil(); i = i->cdr())
    ans += " " + i->car()->str();
  return ans + ")";
}

std::string List::repr() const {
  return "List<" + str() + ">";
}

Type List::type() const {
  return Type::list;
}

const PTR<Sexp> List::car() const {
  return l_car;
}

const PTR<List> List::cdr() const {
  return l_cdr;
}

