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

#include "environment.h"

Env::Env(): scope("global") {}

Env::Env(const std::string& s): scope(s) {}

Env::Env(std::string&& s): scope(s) {}

bool Env::has_var(PTR<Symbol> s) {
  return variable.find(s->get_value()) != variable.end();
}

PTR<Sexp> Env::get_var(PTR<Symbol> s) {
  auto found = variable.find(s->get_value());
  if (found == variable.end())
    return nullptr;
  else
    return found->second;
}

void Env::set_var(PTR<Symbol> s, PTR<Sexp> e) {
  variable[s->get_value()] = e;
}

bool Env::has_fun(PTR<Symbol> s) {
  return function.find(s->get_value()) != variable.end();
}

PTR<Sexp> Env::get_fun(PTR<Symbol> s) {
  auto found = function.find(s->get_value());
  if (found == function.end())
    return nullptr;
  else
    return found->second;
}

void Env::set_fun(PTR<Symbol> s, PTR<Sexp> e) {
  function[s->get_value()] = e;
}

Envs::Envs(PTR<Env> global, std::ostream& o): envs(1, global), os(o) {}

PTR<Sexp> Envs::find_var(PTR<Symbol> s) {
  for (auto i = envs.rbegin(); i != envs.rend(); i++) {
    PTR<Sexp>&& found = (*i)->get_var(s);
    if (found)
      return found;
  }
  throw std::runtime_error("Variable not found");
}

void Envs::set_var(PTR<Symbol> s, PTR<Sexp> e) {
  for (auto i = envs.rbegin(); i != envs.rend(); i++)
    if ((*i)->has_var(s))
      return (*i)->set_var(s, e);
  envs[0]->set_var(s, e);
}

PTR<Sexp> Envs::find_fun(PTR<Symbol> s) {
  for (auto i = envs.rbegin(); i != envs.rend(); i++) {
    PTR<Sexp>&& found = (*i)->get_fun(s);
    if (found)
      return found;
  }
  throw std::runtime_error("Function not found");
}

void Envs::set_fun(PTR<Symbol> s, PTR<Sexp> e) {
  // Always uses the global environment
  envs[0]->set_fun(s, e);
}

void Envs::add_layer(PTR<Env> e) {
  envs.push_back(e);
}

PTR<Env> Envs::top() {
  return envs[envs.size() - 1];
}

std::ostream& Envs::get_os() {
  return os;
}

