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

#include <iostream>

#include "translate.h"
#include "evaluate.h"

int main(int argc, char* argv[]) {
  for (int i = 1; i < argc; i++) {
    PTR<Envs> env = PTR<Envs>(new Envs(PTR<Env>(new Env)));
    for (std::shared_ptr<List> j = parse(argv[i]); !j->nil(); j = j->cdr()) {
      std::cout << j->car()->str() << std::endl;
      PTR<Sexp> e = evaluate(j->car(), env);
      std::cout << e->strtype() << " " << e->str() << std::endl;
    }
  }
}
