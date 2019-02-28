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

#include <cassert>

#include "func.h"

#define DPCS std::dynamic_pointer_cast<Symbol>

Funcs::~Funcs() { /* std::cout << "~Funcs" << std::endl; */ }

Func::Func(std::string n, std::vector<PTR<Symbol>> a, PTR<List> s,
            ENV env): name(n), f_args(a), f_stmt(s), f_env(env) {}

Func::~Func() { /* std::cout << "~Func" << std::endl; */ }

std::string Func::str() const {
  return "#<FUNCTION " + name + ">";
  // LIMIT: similar but not the same as real Lisp
}

const std::string& Func::get_name() const { return name; }

Type Func::type() const { return Type::func; }

size_t Func::get_lb() const { return f_args.size(); }

size_t Func::get_ub() const { return f_args.size(); }

PTR<Sexp> Func::call(std::vector<PTR<Sexp>> args, ENV) {
  assert(args.size() >= f_args.size());
  assert(args.size() <= f_args.size());
  ENV new_env(new Env{f_env, name});
  auto j = f_args.begin();
  for (auto i = args.begin(); i != args.end(); i++, j++)
    new_env->set_var(*j, *i);
  PTR<Sexp> ans = Nil::lisp_nil;
  for (PTR<List> i = f_stmt; !i->nil(); i = i->fcdr())
    ans = evaluate(i->car(), new_env);
  return ans;
}

bool Func::has_tro() const { return true; }

TRInfo Func::call_tro(std::vector<PTR<Sexp>> args, ENV env) const {
  assert(args.size() >= f_args.size());
  assert(args.size() <= f_args.size());
  ENV new_env(new Env{f_env, name});
  auto j = f_args.begin();
  for (auto i = args.begin(); i != args.end(); i++, j++)
    new_env->set_var(*j, *i);
  if (f_stmt->nil())
    return TRInfo(Nil::lisp_nil);
  PTR<List> i = f_stmt;
  for (; !i->fcdr()->nil(); i = i->cdr())
    evaluate(i->car(), new_env);
  return TRInfo(i->car(), new_env);
}

EFunc::EFunc(std::string n, EFUNC_TYPE(f), EFUNC_TRO_TYPE(t)) :
    EFunc(n, f, t, 0) {}

EFunc::EFunc(std::string n, EFUNC_TYPE(f), EFUNC_TRO_TYPE(t), size_t lb) :
    EFunc(n, f, t, lb, std::numeric_limits<std::size_t>::max()) {}

EFunc::EFunc(std::string n, EFUNC_TYPE(f), EFUNC_TRO_TYPE(t), size_t lb,
    size_t hb) :
    name(n), func(f), tro_func(t), lower_bound(lb), upper_bound(hb) {}

EFunc::~EFunc() { /* std::cout << "~EFunc" << std::endl; */ }

std::string EFunc::str() const {
  return "#<FUNCTION " + name + ">";
  // LIMIT: similar but not the same as real Lisp
}

const std::string& EFunc::get_name() const { return name; }

Type EFunc::type() const { return Type::func; }

size_t EFunc::get_lb() const { return lower_bound; }

size_t EFunc::get_ub() const { return upper_bound; }

PTR<Sexp> EFunc::call(std::vector<PTR<Sexp>> args, ENV env) {
  if (args.size() < lower_bound)
    throw std::invalid_argument("Too few arguments");
  if (args.size() > upper_bound)
    throw std::invalid_argument("Too many arguments");
  return func(args, env);
}

bool EFunc::has_tro() const { return tro_func; }

TRInfo EFunc::call_tro(std::vector<PTR<Sexp>> args, ENV env) const {
  return tro_func(args, env);
}

CadrFunc::CadrFunc(std::string n, CADRFUNC_TYPE(f)) : name(n), func(f) {}

CadrFunc::~CadrFunc() { /* std::cout << "~CFunc" << std::endl; */ }

std::string CadrFunc::str() const {
  return "#<FUNCTION " + name + ">";
  // LIMIT: similar but not the same as real Lisp
}

const std::string& CadrFunc::get_name() const { return name; }

Type CadrFunc::type() const { return Type::func; }

size_t CadrFunc::get_lb() const { return 1; }

size_t CadrFunc::get_ub() const { return 1; }

PTR<Sexp> CadrFunc::call(std::vector<PTR<Sexp>> args, ENV env) {
  if (args.size() < 1)
    throw std::invalid_argument("Too few arguments");
  if (args.size() > 1)
    throw std::invalid_argument("Too many arguments");
  return func(name, args, env);
}

bool CadrFunc::has_tro() const { return false; }

TRInfo CadrFunc::call_tro(std::vector<PTR<Sexp>> args, ENV env) const {
  assert(false);
}

