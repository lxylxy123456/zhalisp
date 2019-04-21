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

#include "evaluate.h"
#include "translate.h"

#include <fstream>
#include <iostream>
#include <vector>

void f(Sexp* s) {
  auto env = ENV();
  auto x = evaluate(s, env);
  std::cout << x->str() << std::endl;
}

int main() {
  parse(f);
  auto x = parse("8 9 (8 0)");
  for (auto i : *x)
    std::cout << i->str() << std::endl;
}

