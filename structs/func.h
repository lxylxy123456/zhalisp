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

class Func : public Sexp {
 public:
  Func(std::string, PTR<List>, PTR<List>, PTR<Envs>);
  virtual ~Func();
  virtual std::string str() const;
  virtual std::string repr() const;
  virtual Type type() const;
  PTR<Sexp> call(PTR<List>, PTR<Envs>);
 private:
  std::string name;
  PTR<List> f_args;
  PTR<List> f_stmt;
  PTR<Envs> f_env;
};

#endif
