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

#include "nil.h"
#include "symbol.h"
#include "environment.h"

PTR<Sexp> evaluate(PTR<Sexp> arg, PTR<Envs> env);   // from evaluate.h

class Funcs : public Sexp {
 public:
  virtual ~Funcs();
  virtual std::string str() const = 0;
  virtual Type type() const = 0;
  virtual PTR<Sexp> call(PTR<List>, PTR<Envs>) = 0;
};

class Func : public Funcs {
 public:
  Func(std::string, PTR<List>, PTR<List>, PTR<Envs>);
  virtual ~Func();
  virtual std::string str() const;
  virtual Type type() const;
  virtual PTR<Sexp> call(PTR<List>, PTR<Envs>);
 private:
  std::string name;
  PTR<List> f_args;
  PTR<List> f_stmt;
  PTR<Envs> f_env;
};

class CFunc : public Funcs {
 public:
  CFunc(std::string, PTR<Sexp>(*)(PTR<List>, PTR<Envs>));
  virtual ~CFunc();
  virtual std::string str() const;
  virtual Type type() const;
  virtual PTR<Sexp> call(PTR<List>, PTR<Envs>);
 private:
  std::string name;
  PTR<Sexp>(*func)(PTR<List>, PTR<Envs>);
};

class CadrFunc : public Funcs {
 public:
  CadrFunc(std::string, PTR<Sexp>(*)(std::string, PTR<List>, PTR<Envs>));
  virtual ~CadrFunc();
  virtual std::string str() const;
  virtual Type type() const;
  virtual PTR<Sexp> call(PTR<List>, PTR<Envs>);
 private:
  std::string name;
  PTR<Sexp>(*func)(std::string, PTR<List>, PTR<Envs>);
};

#endif
