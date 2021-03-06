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

#include "sexp.h"

Sexp::~Sexp() { /* std::cout << "~Sexp" << std::endl; */ }

bool Sexp::nil() const { return false; }

bool Sexp::t() const { return true; }

Type Sexp::type() const { return Type::sexp; }

bool Sexp::has_type(Type tid) const {
  static const int type_matrix[] = {
    // ceffnlbacftinss
    // afuuuiotolanuye
    // dunnlsoomottmmx
    // fncsltlmpaiebbp
    0b0000000000000001,   // sexp
    0b0000000010000011,   // symbol
    0b0000000010000101,   // number
    0b0000000010011101,   // integer
    0b0000000010010101,   // rational
    0b0000000010100101,   // float_
    0b0000000011000101,   // complex
    0b0000000010000001,   // atom
    0b0000000110000001,   // boolean
    0b0000001000000001,   // list
    0b0000011110000001,   // null
    0b0000100010000001,   // funcs
    0b0001100010000001,   // func
    0b0010100010000001,   // efunc
    0b0100100010000001,   // cadrfunc
  };
  return type_matrix[static_cast<int>(type())] & (1 << static_cast<int>(tid));
}

const char* Sexp::strtype() {
  static const char * const type_desc[] = {
    "sexp",
    "symbol",
    "number",
    "integer",
    "rational",
    "float",
    "complex",
    "atom",
    "bool",
    "list",
    "null",
    "funcs",
    "func",
    "efunc",
    "cadrfunc",
  };
  return type_desc[static_cast<int>(type())];
}

