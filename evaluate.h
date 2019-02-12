#ifndef EVALUATE_H
#define EVALUATE_H

#include "structs.h"

#define PTR std::shared_ptr
#define DPC std::dynamic_pointer_cast

#define T_ENV void*

PTR<Sexp> evaluate (PTR<Sexp>, T_ENV);

#endif
