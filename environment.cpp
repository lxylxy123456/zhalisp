#include "environment.h"

Env::Env(): scope("global") {}

Env::Env(std::string&& s): scope(s) {}

bool Env::has_var(PTR<Symbol> s) {
  return variable.find(s->get_value()) != variable.end();
}

PTR<Sexp> Env::get_var(PTR<Symbol> s) {
  return variable[s->get_value()];
}

void Env::set_var(PTR<Symbol> s, PTR<Sexp> e) {
  variable[s->get_value()] = e;
}

bool Env::has_fun(PTR<Symbol> s) {
  return function.find(s->get_value()) != variable.end();
}

PTR<Sexp> Env::get_fun(PTR<Symbol> s) {
  return function[s->get_value()];
}

void Env::set_fun(PTR<Symbol> s, PTR<Sexp> e) {
  function[s->get_value()] = e;
}

Envs::Envs(PTR<Env> global): envs(1, global) {}

PTR<Sexp> Envs::find_var(PTR<Symbol> s) {
  for (auto i = envs.rbegin(); i != envs.rend(); i++)
    if ((*i)->has_var(s))
      return (*i)->get_var(s);
  throw std::runtime_error("Variable not found");
}

void Envs::set_var(PTR<Symbol> s, PTR<Sexp> e) {
  for (auto i = envs.rbegin(); i != envs.rend(); i++)
    if ((*i)->has_var(s)) 
      return (*i)->set_var(s, e);
  envs[0]->set_var(s, e);
}

PTR<Sexp> Envs::find_fun(PTR<Symbol> s) {
  for (auto i = envs.rbegin(); i != envs.rend(); i++)
    if ((*i)->has_fun(s))
      return (*i)->get_fun(s);
  throw std::runtime_error("Variable not found");
}

void Envs::set_fun(PTR<Symbol> s, PTR<Sexp> e) {
  // Always uses the global environment
  envs[0]->set_fun(s, e);
}

