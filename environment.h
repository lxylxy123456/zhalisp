#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include <vector>
#include <unordered_map>

#include "structs.h"

class Env {
 public:
  Env();
  Env(std::string&& scope);
  bool has_var(PTR<Symbol>);          // TODO: merge has_ and get_
  PTR<Sexp> get_var(PTR<Symbol>);
  void set_var(PTR<Symbol>, PTR<Sexp>);
  bool has_fun(PTR<Symbol>);
  PTR<Sexp> get_fun(PTR<Symbol>);
  void set_fun(PTR<Symbol>, PTR<Sexp>);

 private:
  std::string scope;
  std::unordered_map<std::string, PTR<Sexp>> variable;
  std::unordered_map<std::string, PTR<Sexp>> function;
};

class Envs {
 public:
  Envs(PTR<Env>);
  PTR<Sexp> find_var(PTR<Symbol>);
  void set_var(PTR<Symbol>, PTR<Sexp>);
  PTR<Sexp> find_fun(PTR<Symbol>);
  void set_fun(PTR<Symbol>, PTR<Sexp>);

 private:
  std::vector<PTR<Env>> envs;
  // TODO: record environment for print
};

#endif
