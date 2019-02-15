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

Func::Func(std::string n, PTR<List> a, PTR<List> s, PTR<Envs> env):
            name(n), f_args(a), f_stmt(s), f_env(env) {}

Func::~Func() {
//  std::cout << "~Func" << std::endl;
}

std::string Func::str() const {
  return "#<FUNCTION " + name + ">";
  // TODO: LIMIT: similar but not the same as real Lisp
}

std::string Func::repr() const {
  return "#<FUNCTION " + name + ">";
}

Type Func::type() const {
  return Type::func;
}

PTR<Sexp> Func::call(PTR<List> args, PTR<Envs> env) {
  PTR<Env> new_env(new Env{name});
  PTR<Envs> new_envs(new Envs{*f_env});
  new_envs->add_layer(new_env);
  for (PTR<List> i = f_args; !i->nil(); i = i->cdr()) {
    if (args->nil())
      throw std::invalid_argument("Not enough actual arguments");
    new_env->set_var(DPCS(i->car()), evaluate(args->car(), env));
    args = args->cdr();
  }
  if (!args->nil())
    throw std::invalid_argument("Too many actual arguments");
  PTR<Sexp> ans = Nil::lisp_nil;
  for (PTR<List> i = f_stmt; !i->nil(); i = i->cdr())
    ans = evaluate(i->car(), new_envs);
  return ans;
}


CFunc::CFunc(std::string n, PTR<Sexp>(*f)(PTR<List>, PTR<Envs>)):
              name(n), func(f) {}

CFunc::~CFunc() {
//  std::cout << "~CFunc" << std::endl;
}

std::string CFunc::str() const {
  return "#<FUNCTION " + name + ">";
  // TODO: LIMIT: similar but not the same as real Lisp
}

std::string CFunc::repr() const {
  return "#<FUNCTION " + name + ">";
}

Type CFunc::type() const {
  return Type::func;
}

PTR<Sexp> CFunc::call(PTR<List> args, PTR<Envs> env) {
  return func(args, env);
}

