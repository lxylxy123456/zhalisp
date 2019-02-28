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

#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include <vector>
#include <unordered_map>

#include "symbol.h"

#define ENV PTR<Env>

class Funcs;        // from func.h

class Env {
 public:
  Env(std::ostream&);
  Env(ENV, const std::string& scope);
  Env(ENV, std::string&& scope);
  PTR<Sexp> find_var(PTR<Symbol> s);
  void set_vars(PTR<Symbol> s, PTR<Sexp> e);
  void set_var(PTR<Symbol> s, PTR<Sexp> e);
  PTR<Funcs> find_fun(PTR<Symbol> s);
  void set_fun(PTR<Symbol> s, PTR<Funcs> e);
  std::ostream& get_os();

 private:
  std::string scope;
  std::unordered_map<std::string, PTR<Sexp>> variable;
  std::unordered_map<std::string, PTR<Funcs>> function;
  PTR<Env> outer;     // global's outer = nullptr
  Env* global;        // global's global = global
  std::ostream& os;
};

#endif
