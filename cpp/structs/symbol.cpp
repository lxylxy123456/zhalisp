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

#include "symbol.h"

#include <algorithm>
#include <cctype>

Symbol::Symbol(const char * const s): value(s) {
  std::transform(value.begin(), value.end(), value.begin(), toupper);
}

Symbol::~Symbol() { /* std::cout << "~Symbol" << std::endl; */ }

std::string Symbol::str() const { return value; }

Type Symbol::type() const { return Type::symbol; }

const std::string& Symbol::get_value() { return value; }

PTR<Symbol> Symbol::lisp_quote(new Symbol("QUOTE"));
PTR<Symbol> Symbol::lisp_function(new Symbol("FUNCTION"));

