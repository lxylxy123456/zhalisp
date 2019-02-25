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

#include "func.h"

#define DPCS std::dynamic_pointer_cast<Symbol>

Funcs::~Funcs() {
//  std::cout << "~Funcs" << std::endl;
}

Func::Func(std::string n, std::vector<PTR<Symbol>> a, PTR<List> s,
            PTR<Envs> env): name(n), f_args(a), f_stmt(s), f_env(env) {}

Func::~Func() {
//  std::cout << "~Func" << std::endl;
}

std::string Func::str() const {
  return "#<FUNCTION " + name + ">";
  // LIMIT: similar but not the same as real Lisp
}

Type Func::type() const {
  return Type::func;
}

size_t Func::get_lb() const {
  return f_args.size();
}

size_t Func::get_ub() const {
  return f_args.size();
}

PTR<Sexp> Func::call(std::vector<PTR<Sexp>> args, PTR<Envs> env) {
  PTR<Env> new_env(new Env{name});
  PTR<Envs> new_envs(new Envs{*f_env});
  new_envs->add_layer(new_env);
  if (args.size() > f_args.size())
    throw std::invalid_argument("Too many actual arguments");
  if (args.size() < f_args.size())
    throw std::invalid_argument("Not enough actual arguments");
  auto j = f_args.begin();
  for (auto i = args.begin(); i != args.end(); i++, j++)
    new_env->set_var(*j, *i);
  PTR<Sexp> ans = Nil::lisp_nil;
  for (PTR<List> i = f_stmt; !i->nil(); i = i->cdr())
    ans = evaluate(i->car(), new_envs);
  return ans;
}

EFunc::EFunc(std::string n, EFUNC_TYPE(f)) : EFunc(n, f, 0) {}

EFunc::EFunc(std::string n, EFUNC_TYPE(f), size_t lb) :
    EFunc(n, f, lb, std::numeric_limits<std::size_t>::max()) {}

EFunc::EFunc(std::string n, EFUNC_TYPE(f), size_t lb, size_t hb) :
    name(n), func(f), lower_bound(lb), upper_bound(hb) {}

EFunc::~EFunc() {
//  std::cout << "~EFunc" << std::endl;
}

std::string EFunc::str() const {
  return "#<FUNCTION " + name + ">";
  // LIMIT: similar but not the same as real Lisp
}

Type EFunc::type() const {
  return Type::func;
}

size_t EFunc::get_lb() const {
  return lower_bound;
}

size_t EFunc::get_ub() const {
  return upper_bound;
}

PTR<Sexp> EFunc::call(std::vector<PTR<Sexp>> args, PTR<Envs> env) {
  return func(args, env);
}

CadrFunc::CadrFunc(std::string n, CADRFUNC_TYPE(f)) : name(n), func(f) {}

CadrFunc::~CadrFunc() {
//  std::cout << "~CFunc" << std::endl;
}

std::string CadrFunc::str() const {
  return "#<FUNCTION " + name + ">";
  // LIMIT: similar but not the same as real Lisp
}

Type CadrFunc::type() const {
  return Type::func;
}

size_t CadrFunc::get_lb() const {
  return 1;
}

size_t CadrFunc::get_ub() const {
  return 1;
}

PTR<Sexp> CadrFunc::call(std::vector<PTR<Sexp>> args, PTR<Envs> env) {
  return func(name, args, env);
}

