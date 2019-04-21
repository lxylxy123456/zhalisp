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

Env::Env(std::ostream& o) :
          scope("global"), outer(nullptr), global(this), os(o) {}

Env::Env(ENV o, const std::string& s) :
          scope(s), outer(o), global(o->global), os(o->os) {}

Env::Env(ENV o, std::string&& s) :
          scope(s), outer(o), global(o->global), os(o->os) {}

PTR<Sexp> Env::find_var(PTR<Symbol> s) {
  const std::string& name = s->get_value();
  for (Env* i = this; i; i = i->outer.get()) {
    auto found = i->variable.find(name);
    if (found != i->variable.end())
      return found->second;
  }
  throw std::runtime_error("Variable not found");
}

void Env::set_vars(PTR<Symbol> s, PTR<Sexp> e) {
  const std::string& name = s->get_value();
  for (Env* i = this; i; i = i->outer.get()) {
    auto found = i->variable.find(name);
    if (found != i->variable.end()) {
      found->second = e;
      return;
    }
  }
  global->variable[name] = e;
}

void Env::set_var(PTR<Symbol> s, PTR<Sexp> e) { variable[s->get_value()] = e; }

PTR<Funcs> Env::find_fun(PTR<Symbol> s) {
  // Always uses the global environment
  auto found = global->function.find(s->get_value());
  if (found != global->function.end())
    return found->second;
  else
    throw std::runtime_error("Function not found");
}

void Env::set_fun(PTR<Symbol> s, PTR<Funcs> e) {
  // Always uses the global environment
  global->function[s->get_value()] = e;
}

std::ostream& Env::get_os() { return os; }

