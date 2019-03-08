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

#include "test.h"

#include "evaluate.h"
#include "translate.h"

#include <fstream>
#include <iostream>
#include <vector>

typedef std::pair<std::string, std::string> Test;
typedef std::vector<std::pair<std::string, std::string>> Tests;

ENV build_test_env(std::ostream& os) {
  return ENV(new Env{os});
}

bool match(PTR<Sexp> a, PTR<Sexp> b) {
  bool str = a->str() == b->str();
//  bool equal = is_equal(a, b);
//  assert(str == equal);
  return str;
}

void read_tests(Tests& tests, const std::string& file_name) {
  std::ifstream ifile(file_name);
  bool state = true;
  for (std::string ln; std::getline(ifile, ln); ) {
    if (!ln.length()) {
      continue;
    } else if (ln.length() >= 2 && ln[0] == '-' && ln[1] == '>') {
      assert(state);
      state = false;
      tests.emplace_back(ln.substr(2), "");
    } else if (ln.length() >= 2 && ln[0] == '=' && ln[1] == '>') {
      assert(!state);
      state = true;
      tests[tests.size() - 1].second += ln.substr(2);
    } else if (!state) {
      tests[tests.size() - 1].first += "\n" + ln;
    } else {
      tests[tests.size() - 1].second += "\n" + ln;
    }
  }
}

void test(const std::string& file_name) {
  {
    Tests tests;
    read_tests(tests, file_name);
    std::ostringstream sout;
    ENV env = build_test_env(sout);

    for (Test& test : tests) {
      std::string& q = test.first;
      std::string& a = test.second;
      if (upper(strip(q)) == "CLEAR-ENV") {
        env = build_test_env(sout);
        std::cout << "-> CLEAR-ENV\n=> CLEAR-ENV\n" << std::endl;
        continue;
      }
      std::shared_ptr<std::vector<PTR<Sexp>>> qp(parse(q)), ap(parse(a));
      auto aa = ap->begin();
      for (auto& qq : *qp) {
        assert(aa != ap->end());
        PTR<Sexp> aaa = *(aa++);
        if (strip(aaa->str()) == "ERROR") {
          bool error_flag = false;
          try {
            std::cout << "-> " << qq->str() << std::endl;
            std::cout << "=> " << evaluate(qq, env)->str() << std::endl;
            SPTR_SWEEP(env);
          } catch (...) {   // https://stackoverflow.com/a/22268788/
            error_flag = true;
            std::cout << "ERROR" << std::endl;
          }
          if (!error_flag) {
            throw std::runtime_error("Test fails: should error");
          }
        } else if (strip(aaa->str()) == "ERRORC") {
          std::cout << "-> " << qq->str() << std::endl;
          std::cout << "=> ERRORC" << std::endl;
        } else {
          sout.str("");
          std::cout << "-> " << qq->str() << std::endl;
          PTR<Sexp> qqq = evaluate(qq, env);
          SPTR_SWEEP(env);
          std::istringstream sin(sout.str());
          for (std::string ln; std::getline(sin, ln); ) {
            std::cout << "p> " << ln << std::endl;
            if (aaa->str() != ln) {
              throw std::runtime_error("Test fails: wrong print output");
            }
            assert(aa != ap->end());
            aaa = *(aa++);
          }
          std::cout << "=> " << qqq->str() << std::endl;
          if (!match(qqq, aaa)) {
            std::cout << "!> " << aaa->str() << std::endl;
            throw std::runtime_error("Test fails: wrong answer");
          }
        }
      }
      assert(aa == ap->end());
      std::cout << std::endl;
    }
  }
  SPTR_SWEEP(ENV{});
}

ENV shell_env = nullptr;

class sig_exit {};

void shell_evaluate(PTR<Sexp> s) {
  if (s->str() == "(EXIT)") {
    throw sig_exit();
  } else if (s->str() == "CLEAR-ENV") {
    shell_env = build_test_env(std::cout);
    std::cout << "=> CLEAR-ENV\n-> ";
    return;
  } else {
    try {
      PTR<Sexp> ans = evaluate(s, shell_env);
      std::cout << "=> " << ans->str() << "\n-> ";
    } catch (std::exception& e) {
      std::cout << "Error: " << e.what() << "\n-> ";
    }
  }
  SPTR_SWEEP(shell_env);
}

void shell() {
  shell_env = build_test_env(std::cout);
  std::cout << "-> ";
  try {
    parse(shell_evaluate);
    std::cout << std::endl;
  } catch (sig_exit& e) {
    // no action
  }
  SPTR_SWEEP(ENV{});
}

