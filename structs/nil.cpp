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

#include "nil.h"

Nil::Nil(): List(nullptr, nullptr) {}

Nil::~Nil() {
//  std::cout << "~Nil" << std::endl;
}

std::string Nil::str() const {
  return "NIL";
}

std::string Nil::repr() const {
  return "Nil<()>";
}

Type Nil::type() const {
  return Type::null;
}

bool Nil::type(Type tid) const {
  return tid == Type::sexp || tid == Type::list || tid == Type::null;
}

bool Nil::nil() const {
  return true;
}

const PTR<Sexp> Nil::car() const {
  return lisp_nil;
}

const PTR<List> Nil::cdr() const {
  return lisp_nil;
}

PTR<Nil> Nil::lisp_nil(new Nil{});

