#ifndef EVALUATE_H
#define EVALUATE_H

#include "structs.h"
#include "environment.h"

#define PTR std::shared_ptr
#define DPC std::dynamic_pointer_cast
#define ENV PTR<Envs>
#define BOOL(X) (X ? (PTR<Sexp>)Bool::lisp_t : (PTR<Sexp>)Nil::lisp_nil)

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

PTR<Sexp> evaluate(PTR<Sexp>, ENV);

#endif
