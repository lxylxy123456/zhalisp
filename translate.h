#ifndef TRANSLATE_H
#define TRANSLATE_H

#include "structs.h"

class SyntaxError: public std::exception {
 public:
  SyntaxError(std::string d);
  std::string desc;
};

std::shared_ptr<List> parse(std::string s);

#endif
