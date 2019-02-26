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

#ifndef FUNC_H
#define FUNC_H

#include <limits>

#include "nil.h"
#include "symbol.h"
#include "environment.h"

#define EFUNC_TYPE(N) PTR<Sexp>(*N)(const std::vector<PTR<Sexp>>&, PTR<Envs>)
#define EFUNC_TRO_TYPE(N) TRInfo(*N)(const std::vector<PTR<Sexp>>&, PTR<Envs>)
#define CADRFUNC_TYPE(N) PTR<Sexp>(*N)(std::string, \
                          const std::vector<PTR<Sexp>>&, PTR<Envs>)

PTR<Sexp> evaluate(PTR<Sexp> arg, PTR<Envs> env);   // from evaluate.h

class TRInfo;

class Funcs : public Sexp {
 public:
  virtual ~Funcs();
  virtual std::string str() const = 0;
  virtual Type type() const = 0;
  virtual const std::string& get_name() const = 0;
  virtual size_t get_lb() const = 0;
  virtual size_t get_ub() const = 0;
  virtual PTR<Sexp> call(std::vector<PTR<Sexp>>, PTR<Envs>) = 0;
  virtual bool has_tro() const = 0;
  virtual TRInfo call_tro(std::vector<PTR<Sexp>>, PTR<Envs>) const = 0;
};

class Func : public Funcs {
 public:
  Func(std::string, std::vector<PTR<Symbol>>, PTR<List>, PTR<Envs>);
  virtual ~Func();
  virtual std::string str() const;
  virtual Type type() const;
  virtual const std::string& get_name() const;
  virtual size_t get_lb() const;
  virtual size_t get_ub() const;
  virtual PTR<Sexp> call(std::vector<PTR<Sexp>>, PTR<Envs>);
  virtual bool has_tro() const;
  virtual TRInfo call_tro(std::vector<PTR<Sexp>>, PTR<Envs>) const;
 private:
  const std::string name;
  const std::vector<PTR<Symbol>> f_args;
  const PTR<List> f_stmt;
  const PTR<Envs> f_env;
};

class EFunc : public Funcs {
 public:
  EFunc(std::string, EFUNC_TYPE(), EFUNC_TRO_TYPE());
  EFunc(std::string, EFUNC_TYPE(), EFUNC_TRO_TYPE(), size_t);
  EFunc(std::string, EFUNC_TYPE(), EFUNC_TRO_TYPE(), size_t, size_t);
  virtual ~EFunc();
  virtual std::string str() const;
  virtual Type type() const;
  virtual const std::string& get_name() const;
  virtual size_t get_lb() const;
  virtual size_t get_ub() const;
  virtual PTR<Sexp> call(std::vector<PTR<Sexp>>, PTR<Envs>);
  virtual bool has_tro() const;
  virtual TRInfo call_tro(std::vector<PTR<Sexp>>, PTR<Envs>) const;
 private:
  const std::string name;
  EFUNC_TYPE(const func);
  EFUNC_TRO_TYPE(const tro_func);
  const size_t lower_bound, upper_bound;
};

class CadrFunc : public Funcs {
 public:
  CadrFunc(std::string, CADRFUNC_TYPE());
  virtual ~CadrFunc();
  virtual std::string str() const;
  virtual Type type() const;
  virtual const std::string& get_name() const;
  virtual size_t get_lb() const;
  virtual size_t get_ub() const;
  virtual PTR<Sexp> call(std::vector<PTR<Sexp>>, PTR<Envs>);
  virtual bool has_tro() const;
  virtual TRInfo call_tro(std::vector<PTR<Sexp>>, PTR<Envs>) const;
 private:
  const std::string name;
  CADRFUNC_TYPE(const func);
};

class TRInfo {
 public:
  TRInfo() : args(nullptr), sexp(nullptr), env(nullptr) {}
  TRInfo(PTR<Sexp> s) : args(nullptr), sexp(s), env(nullptr) {}
  TRInfo(PTR<Sexp> s, PTR<Envs> e) : args(nullptr), sexp(s), env(e) {}
  TRInfo(PTR<Funcs> s, std::vector<PTR<Sexp>>* a, PTR<Envs> e) :
      args(a), sexp(s), env(e) {}

  PTR<std::vector<PTR<Sexp>>> args;   // when eval, NULL; else, parameters
  PTR<Sexp> sexp;                     // when eval, list; else, func
  PTR<Envs> env;                      // environment; when NULL, no eval / call
};

#endif
