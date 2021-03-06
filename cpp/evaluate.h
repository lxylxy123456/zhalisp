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

#ifndef EVALUATE_H
#define EVALUATE_H

#include "structs.h"

#define BOOL(X) ((X) ? (PTR<Sexp>)Bool::lisp_t : (PTR<Sexp>)Nil::lisp_nil)

#define DPCFuncs DPC<Funcs>

bool is_eql(PTR<Sexp> a, PTR<Sexp> b);
bool is_equal(PTR<Sexp> a, PTR<Sexp> b);

std::string strip(const std::string& s);
std::string upper(std::string s);

bool reserved_func(PTR<Symbol> sym);
PTR<Funcs> find_func(PTR<Symbol> sym, ENV env);

PTR<Sexp> evaluate(PTR<Sexp>, ENV);

#endif
