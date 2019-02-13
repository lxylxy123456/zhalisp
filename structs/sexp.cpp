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

Sexp::~Sexp() {
//  std::cout << "~Sexp" << std::endl;
}

Type Sexp::type() const {
  return Type::sexp;
}

bool Sexp::has_type(Type tid) const {
  static const int type_matrix[] = {
    // nlbacftinsds
    // uiotolanuyoe
    // lsoomottmmtx
    // ltlmpaiebb_p
    0b0000000000001,   // sexp
    0b0000000000011,   // dot
    0b0000100000101,   // symbol
    0b0000100001001,   // number
    0b0000100111001,   // integer
    0b0000100101001,   // rational
    0b0000101001001,   // float_
    0b0000110001001,   // complex
    0b0000100000001,   // atom
    0b0001100000001,   // bool_
    0b0010000000001,   // list
    0b0110100000001,   // null
  };
  return type_matrix[static_cast<int>(type())] & (1 << static_cast<int>(tid));
}

const char* Sexp::strtype() {
  static const char * const type_desc[] = {
    "sexp",
    "dot",
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
  };
  return type_desc[static_cast<int>(type())];
}

