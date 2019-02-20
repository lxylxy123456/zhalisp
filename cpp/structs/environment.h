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

class Env {
 public:
  Env();
  Env(const std::string& scope);
  Env(std::string&& scope);
  bool has_var(PTR<Symbol>);
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
  Envs(PTR<Env>, std::ostream&);
  PTR<Sexp> find_var(PTR<Symbol>);
  void set_var(PTR<Symbol>, PTR<Sexp>);
  PTR<Sexp> find_fun(PTR<Symbol>);
  void set_fun(PTR<Symbol>, PTR<Sexp>);
  void add_layer(PTR<Env>);
  PTR<Env> top();
  std::ostream& get_os();

 private:
  std::vector<PTR<Env>> envs;
  std::ostream& os;
};

#endif
