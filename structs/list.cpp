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

List::List(const PTR<Sexp>& a, const PTR<Sexp>& d): l_car(a), l_cdr(d) {}

List::~List() { /* std::cout << "~List" << std::endl; */ }

std::string List::str() const {
  std::string ans = "(" + car()->str();
  PTR<List> i = cdr();
  if (!i)                                 // (1 . 2)
    return ans + " . " + r_cdr()->str() + ")";
  while (!i->nil()) {
    ans += " " + i->car()->str();
    PTR<List> ii = i->cdr();
    if (!ii) {                            // (1 2 3 . 4)
      ans += " . " + i->rw_cdr()->str();
      break;
    }
    i = ii;
  }
  return ans + ")";
}

Type List::type() const { return Type::list; }

const PTR<Sexp> List::car() const { return l_car; }

const PTR<List> List::cdr() const { return DPCL(l_cdr); }

const PTR<List> List::fcdr() const {
  PTR<List>&& ans = DPCL(l_cdr);
  if (!ans)
    throw std::invalid_argument("Dotted list");
  return std::move(ans);
}

const PTR<Sexp> List::r_cdr() const { return l_cdr; }

PTR<Sexp>& List::rw_cdr() { return l_cdr; }

