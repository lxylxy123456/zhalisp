#ifndef EVALUATE_H
#define EVALUATE_H

#include "structs.h"
#include "environment.h"

#define PTR std::shared_ptr
#define DPC std::dynamic_pointer_cast
#define ENV PTR<Envs>

PTR<Sexp> (*find_func(Symbol& sym))(PTR<List>, ENV);

PTR<Sexp> plus(PTR<List>, ENV);
PTR<Sexp> minus(PTR<List>, ENV);
PTR<Sexp> mul(PTR<List>, ENV);
PTR<Sexp> div(PTR<List>, ENV);
PTR<Sexp> setq(PTR<List>, ENV);
PTR<Sexp> one_plus(PTR<List>, ENV);
PTR<Sexp> one_minus(PTR<List>, ENV);

PTR<Sexp> evaluate (PTR<Sexp>, ENV);

#endif
