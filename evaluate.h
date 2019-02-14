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
#include "environment.h"

#define PTR std::shared_ptr
#define ENV PTR<Envs>
#define BOOL(X) (X ? (PTR<Sexp>)Bool::lisp_t : (PTR<Sexp>)Nil::lisp_nil)

#define DPC std::dynamic_pointer_cast
#define DPCN DPC<Number>
#define DPCS DPC<Symbol>
#define DPCL DPC<List>

PTR<Sexp> (*find_func(PTR<Symbol> sym))(PTR<List>, ENV);

PTR<Sexp> plus(PTR<List>, ENV);
PTR<Sexp> minus(PTR<List>, ENV);
PTR<Sexp> mul(PTR<List>, ENV);
PTR<Sexp> div(PTR<List>, ENV);
PTR<Sexp> setq(PTR<List>, ENV);
PTR<Sexp> one_plus(PTR<List>, ENV);
PTR<Sexp> one_minus(PTR<List>, ENV);
PTR<Sexp> eq_(PTR<List>, ENV);
PTR<Sexp> lt(PTR<List>, ENV);
PTR<Sexp> le(PTR<List>, ENV);
PTR<Sexp> gt(PTR<List>, ENV);
PTR<Sexp> ge(PTR<List>, ENV);
PTR<Sexp> sqrt_(PTR<List>, ENV);
PTR<Sexp> atom(PTR<List>, ENV);
PTR<Sexp> listp(PTR<List>, ENV);
PTR<Sexp> null(PTR<List>, ENV);
PTR<Sexp> numberp(PTR<List>, ENV);
PTR<Sexp> typep(PTR<List>, ENV);
PTR<Sexp> symbolp(PTR<List>, ENV);
PTR<Sexp> zerop(PTR<List>, ENV);
PTR<Sexp> evenp(PTR<List>, ENV);
PTR<Sexp> oddp(PTR<List>, ENV);
PTR<Sexp> eq(PTR<List>, ENV);
PTR<Sexp> eql(PTR<List>, ENV);
PTR<Sexp> equal(PTR<List>, ENV);

PTR<Sexp> evaluate(PTR<Sexp>, ENV);

#endif
