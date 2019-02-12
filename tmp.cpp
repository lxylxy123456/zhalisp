#include <iostream>

#include "translate.h"
#include "evaluate.h"

int main(int argc, char* argv[]) {
  for (int i = 1; i < argc; i++) {
    PTR<Envs> env = PTR<Envs>(new Envs(PTR<Env>(new Env)));
    for (std::shared_ptr<List> j = parse(argv[i]); !j->nil(); j = j->cdr()) {
      std::cout << j->car()->str() << std::endl;
      PTR<Sexp> e = evaluate(j->car(), env);
      std::cout << e->strtype() << " " << e->str() << std::endl;
    }
  }
}
