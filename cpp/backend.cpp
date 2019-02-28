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

void shell() {
  ENV env(new Env{std::cout});
  while (true) {
    std::cout << "-> " << std::flush;
    std::string lns;
    std::getline(std::cin, lns);
    if (std::cin.eof()) {
      std::cout << std::endl;
      break;
    }
    PTR<List> tree;
    while (true) {
      try {
        tree = parse(lns);
        break;
      } catch (SyntaxError& e) {
        std::string ln;
        std::getline(std::cin, ln);
        lns += "\n" + ln;
      }
    }
    if (upper(strip(lns)) == "(EXIT)") {
      break;
    } else if (upper(strip(lns)) == "CLEAR-ENV") {
      env = ENV(new Env{std::cout});
      std::cout << "=> CLEAR-ENV" << std::endl;
      continue;
    }
    for (PTR<List> i = tree; !i->nil(); i = i->cdr()) {
      try {
        PTR<Sexp> ans = evaluate(i->car(), env);
        std::cout << "=> " << ans->str() << std::endl;
      } catch (std::exception& e) {
        std::cout << "Error: " << e.what() << std::endl;
      }
    }
  }
}

int main(int argc, char* argv[]) {
  shell();
}
