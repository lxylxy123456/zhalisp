#include <iostream>

#include "structs.cpp"
#include "translate.h"

int main(int argc, char* argv[]) {
  for (int i = 1; i < argc; i++) {
    std::shared_ptr<List> exp = std::dynamic_pointer_cast<List>(parse(argv[i]));
    auto _1 = std::dynamic_pointer_cast<Number>(exp->car());
    auto _2 = std::dynamic_pointer_cast<Number>(exp->cdr()->car());
    std::cout << _1->strtype() << " " << _2->strtype() << std::endl;
    auto _3 = _1->operator+(*_2);
    std::cout << _1->str() << " " << _2->str() << " " << _3->str() << std::endl;
  }
}
