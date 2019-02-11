#include <iostream>

#include "translate.h"
#include "evaluate.h"

int main(int argc, char* argv[]) {
  for (int i = 1; i < argc; i++)
    for (std::shared_ptr<List> j = parse(argv[i]); !j->nil(); j = j->cdr()) {
      PTR<Sexp> e = evaluate(j->car(), nullptr);
      std::cout << e->strtype() << " " << e->str() << std::endl;
    }
}
