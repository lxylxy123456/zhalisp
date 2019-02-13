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

#include "bool.h"

Bool::Bool(const char * const s) {
  if (s[1] != '\0' || (s[0] != 't' && s[0] != 'T'))
    throw std::invalid_argument("Incorrect type name");
}

Bool::~Bool() {
//  std::cout << "~Bool" << std::endl;
}

std::string Bool::str() const {
  return "T";
}

std::string Bool::repr() const {
  return "Bool<T>";
}

Type Bool::type() const {
  return Type::bool_;
}

bool Bool::type(Type tid) const {
  return tid == Type::sexp || tid == Type::atom;
}

PTR<Bool> Bool::lisp_t(new Bool{"T"});

