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

#include <exception>
#include <unordered_map>
#include <regex>

// Conversion

// TODO: remove use of DPC? (without check)

PTR<Number> FDPCN(PTR<Sexp> p) {
  PTR<Number>&& ans = DPC<Number>(p);
  if (!ans)
    throw std::invalid_argument("Not a number");
  return ans;
}

PTR<Symbol> FDPCS(PTR<Sexp> p) {
  PTR<Symbol>&& ans = DPC<Symbol>(p);
  if (!ans)
    throw std::invalid_argument("Not a symbol");
  return ans;
}

PTR<Integer> FDPCI(PTR<Sexp> p) {
  PTR<Integer>&& ans = DPC<Integer>(p);
  if (!ans)
    throw std::invalid_argument("Not an integer");
  return ans;
}

PTR<List> FDPCL(PTR<Sexp> p) {
  PTR<List>&& ans = DPC<List>(p);
  if (!ans)
    throw std::invalid_argument("Not an list");
  return ans;
}

// Helper functions

bool is_eql(PTR<Sexp> a, PTR<Sexp> b) {
  Type ta = a->type();
  if (ta != b->type())
    return false;
  switch (ta) {
  case Type::integer :
    return *DPCI(a) == *DPCI(b);
  case Type::rational :
    return *DPCR(a) == *DPCR(b);
  case Type::float_ :
    return *DPCF(a) == *DPCF(b);
  case Type::complex :
    return *DPCC(a) == *DPCC(b);
  case Type::list :
    return a.get() == b.get();
  case Type::symbol :
    return DPCS(a)->get_value() == DPCS(b)->get_value();
  case Type::null :
  case Type::boolean :
    return true;
  default :
    throw std::invalid_argument("Invalid type");
  }
}

bool is_equal(PTR<Sexp> a, PTR<Sexp> b) {
  Type ta = a->type();
  if (ta != b->type())
    return false;
  switch (ta) {
  case Type::integer :
    return *DPCI(a) == *DPCI(b);
  case Type::rational :
    return *DPCR(a) == *DPCR(b);
  case Type::float_ :
    return *DPCF(a) == *DPCF(b);
  case Type::complex :
    return *DPCC(a) == *DPCC(b);
  case Type::list : {
    PTR<List> la = DPCL(a), lb = DPCL(b);
    return is_equal(la->car(), lb->car()) && is_equal(la->r_cdr(), lb->r_cdr());
  }
  case Type::symbol :
    return DPCS(a)->get_value() == DPCS(b)->get_value();
  case Type::null :
  case Type::boolean :
    return true;
  default :
    throw std::invalid_argument("Invalid type");
  }
}

PTR<Funcs> sym_to_func(PTR<Sexp> s, ENV env) {
  // If symbol, resolve to function; else, cast to function
  PTR<Symbol> sym = DPC<Symbol>(s);
  if (sym)
    return find_func(sym, env);
  else
    return DPC<Funcs>(s);
  // TODO: build in error when function not found
}

std::string strip(const std::string& s) {
  size_t f = s.find_first_not_of(" \n\r\t");
  size_t l = s.find_last_not_of(" \n\r\t");
  if (f != s.npos)
    return s.substr(f, l - f + 1);
  assert(l == s.npos);
  return "";
}

std::string upper(std::string s) {
  std::transform(s.begin(), s.end(), s.begin(), toupper);
  return s;
}

#define ARGS_SIZE_LB(X) assert(args.size() >= X);
#define ARGS_SIZE_UB(X) assert(args.size() <= X);
#define ARGS_SIZE_EQ(X) ARGS_SIZE_LB(X) ARGS_SIZE_UB(X)

// Arithmetics

PTR<Sexp> plus(const std::vector<PTR<Sexp>>& args, ENV env) {
  PTR<Number> ans = Integer::lisp_0;
  for (auto& i : args)
    ans = (*ans) + (*FDPCN(i));
  return ans;
}

PTR<Sexp> minus(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_LB(1)
  if (args.size() == 1)
    return -(*FDPCN(args[0]));
  PTR<Number> ans = FDPCN(args[0]);
  for (auto i = args.begin() + 1; i != args.end(); i++) {
    ans = (*ans) - (*FDPCN(*i));
  }
  return ans;
}

PTR<Sexp> mul(const std::vector<PTR<Sexp>>& args, ENV env) {
  PTR<Number> ans = Integer::lisp_1;
  for (auto& i : args)
    ans = (*ans) * (*FDPCN(i));
  return ans;
}

PTR<Sexp> div(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_LB(1)
  if (args.size() == 1)
    return (*Integer::lisp_1) / (*FDPCN(args[0]));
  PTR<Number> ans = FDPCN(args[0]);
  for (auto i = args.begin() + 1; i != args.end(); i++) {
    ans = (*ans) / (*FDPCN(*i));
  }
  return ans;
}

PTR<Sexp> one_plus(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return (*FDPCN(args[0])) + (*Integer::lisp_1);
}

PTR<Sexp> one_minus(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return (*FDPCN(args[0])) - (*Integer::lisp_1);
}

PTR<Sexp> eq_(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  return BOOL((*FDPCN(args[0])) == (*FDPCN(args[1])));
}

PTR<Sexp> lt(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  return BOOL((*FDPCN(args[0])) < (*FDPCN(args[1])));
}

PTR<Sexp> le(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  return BOOL((*FDPCN(args[0])) <= (*FDPCN(args[1])));
}

PTR<Sexp> gt(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  return BOOL((*FDPCN(args[0])) > (*FDPCN(args[1])));
}

PTR<Sexp> ge(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  return BOOL((*FDPCN(args[0])) >= (*FDPCN(args[1])));
}

PTR<Sexp> sqrt_(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return FDPCN(args[0])->sqrt_();
}

// Unary Predicates

PTR<Sexp> atom(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return BOOL(args[0]->has_type(Type::atom));
}

PTR<Sexp> listp(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return BOOL(args[0]->has_type(Type::list));
}

PTR<Sexp> null(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return BOOL(args[0]->has_type(Type::null));
}

PTR<Sexp> numberp(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return BOOL(args[0]->has_type(Type::number));
}

PTR<Sexp> typep(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  // Does not supporting 3rd arg
  PTR<Sexp> value = args[0];
  std::string type_name = FDPCS(args[1])->get_value();
  static std::unordered_map<std::string, Type> lookup_type = {
    {"ATOM", Type::atom},
    {"LIST", Type::list},
    {"NUMBER", Type::number},
    {"INTEGER", Type::integer},
    {"RATIONAL", Type::rational},
    {"FLOAT", Type::float_},
    {"COMPLEX", Type::complex},
    {"SYMBOL", Type::symbol},
    {"BOOLEAN", Type::boolean},
  };
  auto found = lookup_type.find(type_name);
  if (found != lookup_type.end())
    return BOOL(args[0]->has_type(found->second));
  else if (type_name == "NIL")
    return BOOL(false);
  else
    throw std::invalid_argument("Invalid type specifier or not implemented");
}

PTR<Sexp> symbolp(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return BOOL(args[0]->has_type(Type::symbol));
}

PTR<Sexp> zerop(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return BOOL(FDPCN(args[0])->is_0());
}

PTR<Sexp> evenp(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return BOOL(FDPCI(args[0])->get_value() % 2 == 0);
}

PTR<Sexp> oddp(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return BOOL(FDPCI(args[0])->get_value() % 2 == 1);
}

// Binary Predicates

PTR<Sexp> eq(const std::vector<PTR<Sexp>>& args, ENV env) {
  // Same as eql
  ARGS_SIZE_EQ(2)
  return BOOL(is_eql(args[0], args[1]));
}

PTR<Sexp> eql(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  return BOOL(is_eql(args[0], args[1]));
}

PTR<Sexp> equal(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  return BOOL(is_equal(args[0], args[1]));
}

// Logic

PTR<Sexp> and_(PTR<List> args, ENV env) {
  if (!args || args->nil())
    return BOOL(true);
  PTR<Sexp> car;
  while ((car = evaluate(args->car(), env))->t() && (args = args->cdr()) &&
          args->t()) {}
  return car;
}

PTR<Sexp> or_(PTR<List> args, ENV env) {
  if (!args || args->nil())
    return BOOL(false);
  PTR<Sexp> car;
  while ((car = evaluate(args->car(), env))->nil() && (args = args->cdr()) &&
          args->t()) {}
  return car;
}

PTR<Sexp> not_(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return BOOL(args[0]->nil());
}

// List operations

PTR<Sexp> car(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return FDPCL(args[0])->car();
}

PTR<Sexp> cdr(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return FDPCL(args[0])->r_cdr();
}

PTR<Sexp> caordr(std::string name, const std::vector<PTR<Sexp>>& args,
                  ENV env) {
  ARGS_SIZE_EQ(1)
  PTR<Sexp> ans = args[0];
  for (auto i = name.rbegin() + 1; i + 1 != name.rend(); i++) {
    if (*i == 'A')
      ans = FDPCL(ans)->car();
    else
      ans = FDPCL(ans)->r_cdr();
  }
  return ans;
}

PTR<Sexp> cons(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  return PTRNL(args[0], args[1]);
}

PTR<Sexp> list_(const std::vector<PTR<Sexp>>& args, ENV env) {
  PTR<Sexp> ans = Nil::lisp_nil;
  PTR<Sexp>* next_ins = &ans;
  for (auto& i : args) {
    *next_ins = PTRNL(i, Nil::lisp_nil);
    next_ins = &(FDPCL(*next_ins))->rw_cdr();
  }
  return ans;
}

PTR<Sexp> member(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  PTR<Sexp> x = args[0];
  PTR<List> l = FDPCL(args[1]);
  while (!l->nil()) {
    if (is_eql(x, l->car()))
      return l;
    else
      l = l->cdr();   // TODO: can cause segfault (check all List::cdr calls)
  }
  return l;
}

// High-Order Functions

PTR<Sexp> mapcar(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_LB(2)
  PTR<Funcs> func = sym_to_func(args[0], env);
  if (!func)
    throw std::invalid_argument("Not a function");
  std::vector<PTR<List>> arg_list;
  for (auto i = args.begin() + 1; i != args.end(); i++)
    arg_list.push_back(FDPCL(*i));
  PTR<Sexp> ans = Nil::lisp_nil;
  PTR<Sexp>* next_ans = &ans;
  while (true) {
    std::vector<PTR<Sexp>> param;
    for (auto &i : arg_list) {
      if (i->nil())
        return ans;
      param.push_back(i->car());
      i = i->cdr();
    }
    PTR<Sexp> ans_next = func->call(param, env);
    *next_ans = PTRNL(ans_next, Nil::lisp_nil);
    next_ans = &(DPCL(*next_ans))->rw_cdr();
  }
}

PTR<Sexp> mapc(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_LB(2)
  PTR<Funcs> func = sym_to_func(args[0], env);
  if (!func)
    throw std::invalid_argument("Not a function");
  std::vector<PTR<List>> arg_list;
  for (auto i = args.begin() + 1; i != args.end(); i++)
    arg_list.push_back(FDPCL(*i));
  while (true) {
    std::vector<PTR<Sexp>> param;
    for (auto &i : arg_list) {
      if (i->nil())
        return args[1];
      param.push_back(i->car());
      i = i->cdr();
    }
    func->call(param, env);
  }
}

PTR<Sexp> maplist(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_LB(2)
  PTR<Funcs> func = sym_to_func(args[0], env);
  if (!func)
    throw std::invalid_argument("Not a function");
  std::vector<PTR<List>> arg_list;
  for (auto i = args.begin() + 1; i != args.end(); i++)
    arg_list.push_back(FDPCL(*i));
  PTR<Sexp> ans = Nil::lisp_nil;
  PTR<Sexp>* next_ans = &ans;
  while (true) {
    std::vector<PTR<Sexp>> param;
    for (auto &i : arg_list) {
      if (i->nil())
        return ans;
      param.push_back(i);
      i = i->cdr();
    }
    PTR<Sexp> ans_next = func->call(param, env);
    *next_ans = PTRNL(ans_next, Nil::lisp_nil);
    next_ans = &(DPCL(*next_ans))->rw_cdr();
  }
}

PTR<Sexp> append(const std::vector<PTR<Sexp>>& args, ENV env) {
  PTR<Sexp> ans = Nil::lisp_nil;
  PTR<Sexp>* next_ans = &ans;
  for (auto& i : args) {
    PTR<Sexp> j = i;
    while (true) {
      if (!(*next_ans)->nil())
        throw std::invalid_argument("Dotted list");
      if (j->has_type(Type::atom)) {
        *next_ans = j;
        break;
      } else {
        PTR<List> jj = DPCL(j);
        PTR<List> nlist = PTRNL(jj->car(), Nil::lisp_nil);
        *next_ans = nlist;
        next_ans = &nlist->rw_cdr();
        j = jj->r_cdr();
      }
    }
  }
  return ans;
}

// Functions

PTR<Sexp> defun(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  PTR<Symbol> f_name = DPCS(args->car());
  if (!f_name)
    throw std::invalid_argument("Invalid function name");
  if (reserved_func(f_name))
    throw std::invalid_argument("Reserved function name");
  std::vector<PTR<Symbol>> f_args;
  for (PTR<List> i = FDPCL(args->cdr()->car()); !i->nil();
        i = FDPCL(i->r_cdr()))
    f_args.push_back(FDPCS(i->car()));
  PTR<List> f_stmt = DPCL(args->cdr()->cdr());
  PTR<Func> func(new Func(f_name->get_value(), f_args, f_stmt, env));
  env->set_fun(f_name, func);
  return f_name;
}

PTR<Sexp> lambda_(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  std::vector<PTR<Symbol>> f_args;
  for (PTR<List> i = FDPCL(args->car()); !i->nil(); i = FDPCL(i->r_cdr()))
    f_args.push_back(FDPCS(i->car()));
  PTR<List> f_stmt = DPCL(args->cdr());
  PTR<Func> func(new Func("LAMBDA", f_args, f_stmt, env));
  return func;
}

PTR<Sexp> apply(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_LB(2)
  PTR<Funcs> func = sym_to_func(args[0], env);
  if (!func)
    throw std::invalid_argument("Not a function");
  std::vector<PTR<Sexp>> arg;
  auto i = args.begin();
  for (i++; i + 1 != args.end(); i++)
    arg.push_back(*i);
  PTR<List> last_list = FDPCL(*i);
  for (PTR<List> j = last_list; !j->nil(); j = j->cdr())
    arg.push_back(j->car());
  return func->call(arg, env);
}

PTR<Sexp> funcall(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_LB(1)
  PTR<Funcs> func = sym_to_func(args[0], env);
  if (!func)
    throw std::invalid_argument("Not a function");
  std::vector<PTR<Sexp>> arg;
  auto i = args.begin();
  for (i++; i != args.end(); i++)
    arg.push_back(*i);
  return func->call(arg, env);
}

PTR<Sexp> function(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Symbol> as = DPCS(args->car());
  if (as) {
    return find_func(as, env);
  } else {
    assert(DPCS(DPCL(args->car())->car())->get_value() == "LAMBDA");
    return evaluate(args->car(), env);
  }
}

PTR<Sexp> quote(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return args->car();
}

PTR<Sexp> eval_(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return evaluate(args[0], env);
}

// Variables

PTR<Sexp> let(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  PTR<Env> new_env(new Env{"LET"});
  PTR<Envs> new_envs(new Envs{*env});
  new_envs->add_layer(new_env);
  for (PTR<List> i = DPCL(args->car()); i && !i->nil(); i = i->cdr()) {
    PTR<Symbol> k = DPCS(i->car());
    PTR<Sexp> v = Nil::lisp_nil;
    if (!k) {
      PTR<List> il = DPCL(i->car());
      if (!il)
        throw std::invalid_argument("Not a variable name");
      if (!il->cdr()->cdr()->nil())
        throw std::invalid_argument("Argument too long");
      k = DPCS(il->car());
      v = evaluate(il->cdr()->car(), env);
    }
    new_env->set_var(k, v);
  }
  PTR<Sexp> ans = Nil::lisp_nil;
  for (PTR<List> i = args->cdr(); i && !i->nil(); i = i->cdr())
    ans = evaluate(i->car(), new_envs);
  return ans;
}

PTR<Sexp> let_star(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  PTR<Env> new_env(new Env{"LET*"});
  PTR<Envs> new_envs(new Envs{*env});
  new_envs->add_layer(new_env);
  for (PTR<List> i = DPCL(args->car()); i && !i->nil(); i = i->cdr()) {
    PTR<Symbol> k = DPCS(i->car());
    PTR<Sexp> v = Nil::lisp_nil;
    if (!k) {
      PTR<List> il = DPCL(i->car());
      if (!il)
        throw std::invalid_argument("Not a variable name");
      if (!il->cdr()->cdr()->nil())
        throw std::invalid_argument("Argument too long");
      k = DPCS(il->car());
      v = evaluate(il->cdr()->car(), new_envs);
    }
    new_env->set_var(k, v);
  }
  PTR<Sexp> ans = Nil::lisp_nil;
  for (PTR<List> i = args->cdr(); i && !i->nil(); i = i->cdr())
    ans = evaluate(i->car(), new_envs);
  return ans;
}

PTR<Sexp> setq(PTR<List> args, ENV env) {
  int arg_count = 0;
  for (PTR<List> i = args; !i->nil(); i = i->cdr())
    arg_count++;
  if (arg_count % 2)
    throw std::invalid_argument("Odd number of arguments");
  PTR<Sexp> v = Nil::lisp_nil;
  for (PTR<List> i = args; !i->nil(); i = i->cdr()) {
    PTR<Symbol> k = DPCS(i->car());
    if (!k)
      throw std::invalid_argument("Not symbol");
    i = i->cdr();
    v = evaluate(i->car(), env);
    env->set_var(k, v);
  }
  return v;
}

PTR<Sexp> set(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  env->set_var(FDPCS(args[0]), args[1]);
  return args[1];
}

// Conditions

PTR<Sexp> cond(PTR<List> args, ENV env) {
  for (PTR<List> i = args; i && !i->nil(); i = i->cdr()) {
    PTR<List> test = DPCL(i->car());
    if (!test || test->nil())
      throw std::invalid_argument("Should pass a list");
    PTR<Sexp> ans = evaluate(test->car(), env);
    if (ans->t()) {
      for (PTR<List> j = test->cdr(); j && !j->nil(); j = j->cdr())
        ans = evaluate(j->car(), env);
      return ans;
    }
  }
  return Nil::lisp_nil;
}

PTR<Sexp> if_(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->cdr()->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  if (evaluate(args->car(), env)->t())
    return evaluate(args->cdr()->car(), env);
  else
    return evaluate(args->cdr()->cdr()->car(), env);
}

// Iteration

PTR<Sexp> do_(PTR<List> args, ENV env) {
  if (args->cdr()->nil())
    throw std::invalid_argument("Too few arguments");
  PTR<Env> new_env(new Env{"DO"});
  PTR<Envs> new_envs(new Envs{*env});
  new_envs->add_layer(new_env);
  std::vector<std::pair<PTR<Symbol>, PTR<Sexp>>> val_list;
  for (PTR<List> i = DPCL(args->car()); !i->nil(); i = i->cdr()) {
    PTR<List> li = DPCL(i->car());
    if (li) {
      assert(li->cdr()->cdr()->cdr()->nil());
      PTR<Symbol> name = DPCS(li->car());
      new_env->set_var(name, evaluate(li->cdr()->car(), env));
      if (!li->cdr()->cdr()->nil())
        val_list.emplace_back(name, li->cdr()->cdr()->car());
    } else {
      new_env->set_var(DPCS(i->car()), Nil::lisp_nil);
    }
  }
  PTR<Sexp> exit_test = DPCL(args->cdr()->car())->car();
  while (evaluate(exit_test, new_envs)->nil()) {
    for (PTR<List> i = args->cdr()->cdr(); !i->nil(); i = i->cdr())
      evaluate(i->car(), new_envs);
    std::vector<std::pair<PTR<Symbol>, PTR<Sexp>>> new_values;
    for (auto i = val_list.begin(); i != val_list.end(); i++)
      new_values.emplace_back(i->first, evaluate(i->second, new_envs));
    for (auto i = new_values.begin(); i != new_values.end(); i++)
      new_env->set_var(i->first, i->second);
  }
  PTR<Sexp> ans = Nil::lisp_nil;
  for (PTR<List> i = DPCL(args->cdr()->car())->cdr(); !i->nil(); i = i->cdr())
    ans = evaluate(i->car(), new_envs);
  return ans;
}

class ProgInterrupt: public std::exception {
 public:
  ProgInterrupt(int t, PTR<Env> e, PTR<Sexp> v): type(t), env(e), value(v) {}
  int type;       // 1: go; 2: return
  PTR<Env> env;
  PTR<Sexp> value;
  virtual const char* what() const noexcept { return "ProgInterrupt"; }
};

PTR<Sexp> prog(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  PTR<Env> new_env(new Env{"PROG"});
  PTR<Envs> new_envs(new Envs{*env});
  new_envs->add_layer(new_env);
  for (PTR<List> i = DPCL(args->car()); !i->nil(); i = i->cdr()) {
    PTR<List> li = DPCL(i->car());
    if (li) {
      assert(li->cdr()->cdr()->nil());
      new_env->set_var(DPCS(li->car()), evaluate(li->cdr()->car(), env));
    } else {
      new_env->set_var(DPCS(i->car()), Nil::lisp_nil);
    }
  }
  std::vector<PTR<Sexp>> sequence;
  std::vector<std::pair<std::string, size_t>> labels;
  for (PTR<List> i = args->cdr(); i && !i->nil(); i = i->cdr()) {
    PTR<List> li = DPCL(i->car());
    if (li)
      sequence.push_back(li);
    else
      labels.emplace_back(i->car()->str(), sequence.size());
  }
  for (size_t cur = 0; cur < sequence.size(); ) {
    try {
      evaluate(sequence[cur], new_envs);
    } catch (ProgInterrupt& e) {
      if (e.env.get() != new_env.get())
        throw e;
      if (e.type == 1) {
        cur = std::numeric_limits<std::size_t>::max();
        for (auto i = labels.begin(); i != labels.end(); i++) {
          if (i->first == e.value->str()) {
            cur = i->second;
            break;
          }
        }
        if (cur == std::numeric_limits<std::size_t>::max())
          throw std::invalid_argument("Label not found");
        continue;
      } else if (e.type == 2) {
        return e.value;
      } else {
        throw std::invalid_argument("Invalid interrupt number");
      }
    }
    cur++;
  }
  return Nil::lisp_nil;
}

PTR<Sexp> go(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  throw ProgInterrupt{1, env->top(), args->car()};
}

PTR<Sexp> return_(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->cdr()->nil())
    throw std::invalid_argument("Too many arguments");
  throw ProgInterrupt{2, env->top(), evaluate(args->car(), env)};
}

// I/O

PTR<Sexp> print_(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  env->get_os() << args[0]->str() << std::endl;
  return args[0];
}

// Special functions

PTR<Sexp> setrecursionlimit(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return args[0];
}

// Evaluate

#define REGISTER_EFUNC(K, ...) {K, PTR<EFunc>(new EFunc(K, __VA_ARGS__))},

std::unordered_map<std::string, PTR<EFunc>> efmap = {
  REGISTER_EFUNC("+", plus)
  REGISTER_EFUNC("-", minus, 1)
  REGISTER_EFUNC("*", mul)
  REGISTER_EFUNC("/", div, 1)
  REGISTER_EFUNC("1+", one_plus, 1, 1)
  REGISTER_EFUNC("1-", one_minus, 1, 1)
  REGISTER_EFUNC("=", eq_, 2, 2)
  REGISTER_EFUNC("<", lt, 2, 2)
  REGISTER_EFUNC("<=", le, 2, 2)
  REGISTER_EFUNC(">", gt, 2, 2)
  REGISTER_EFUNC(">=", ge, 2, 2)
  REGISTER_EFUNC("SQRT", sqrt_, 1, 1)
  REGISTER_EFUNC("ATOM", atom, 1, 1)
  REGISTER_EFUNC("LISTP", listp, 1, 1)
  REGISTER_EFUNC("NULL", null, 1, 1)
  REGISTER_EFUNC("NUMBERP", numberp, 1, 1)
  REGISTER_EFUNC("TYPEP", typep, 2, 2)
  REGISTER_EFUNC("SYMBOLP", symbolp, 1, 1)
  REGISTER_EFUNC("ZEROP", zerop, 1, 1)
  REGISTER_EFUNC("EVENP", evenp, 1, 1)
  REGISTER_EFUNC("ODDP", oddp, 1, 1)
  REGISTER_EFUNC("EQ", eq, 2, 2)
  REGISTER_EFUNC("EQL", eql, 2, 2)
  REGISTER_EFUNC("EQUAL", equal, 2, 2)
  REGISTER_EFUNC("NOT", not_, 1, 1)
  REGISTER_EFUNC("CAR", car, 1, 1)
  REGISTER_EFUNC("CDR", cdr, 1, 1)
  REGISTER_EFUNC("CONS", cons, 2, 2)
  REGISTER_EFUNC("LIST", list_)
  REGISTER_EFUNC("MEMBER", member, 2, 2)
  REGISTER_EFUNC("MAPCAR", mapcar, 2)
  REGISTER_EFUNC("MAPC", mapc, 2)
  REGISTER_EFUNC("MAPLIST", maplist, 2)
  REGISTER_EFUNC("APPEND", append)
  REGISTER_EFUNC("APPLY", apply, 2)
  REGISTER_EFUNC("FUNCALL", funcall, 1)
  REGISTER_EFUNC("EVAL", eval_, 1, 1)
  REGISTER_EFUNC("SET", set, 2, 2)
  REGISTER_EFUNC("PRINT", print_, 1, 1)
  REGISTER_EFUNC("SETRECURSIONLIMIT", setrecursionlimit, 1, 1)
};

#define CFUNC_TYPE(N) PTR<Sexp>(*N)(PTR<List>, PTR<Envs>)

std::unordered_map<std::string, CFUNC_TYPE()> fmap = {
  {"AND", and_},
  {"OR", or_},
  {"DEFUN", defun},
  {"LAMBDA", lambda_},
  {"FUNCTION", function},
  {"QUOTE", quote},
  {"LET", let},
  {"LET*", let_star},
  {"SETQ", setq},
  {"COND", cond},
  {"IF", if_},
  {"DO", do_},
  {"PROG", prog},
  {"GO", go},
  {"RETURN", return_},
};

bool reserved_func(PTR<Symbol> sym) {
  const std::string& fun_name = sym->get_value();
  if (fmap.find(fun_name) != fmap.end())
    return true;
  if (efmap.find(fun_name) != efmap.end())
    return true;
  static const std::regex re_caordr("C[AD]{1,4}R");
  if (std::regex_match(fun_name, re_caordr))
    return true;
  return false;
}

CFUNC_TYPE(find_special_func(PTR<Symbol> sym)) {
  auto found = fmap.find(sym->get_value());
  if (found == fmap.end())
    return nullptr;
  else
    return found->second;
}

PTR<Funcs> find_func(PTR<Symbol> sym, ENV env) {
  const std::string& fun_name = sym->get_value();
  auto found = efmap.find(fun_name);
  if (found != efmap.end())
    return found->second;
  static const std::regex re_caordr("C[AD]{1,4}R");
  if (std::regex_match(fun_name, re_caordr))
    return PTR<CadrFunc>(new CadrFunc(fun_name, caordr));
  PTR<Funcs> func = DPC<Funcs>(env->find_fun(sym));
  return func;
}

PTR<Sexp> evaluate(PTR<Sexp> arg, ENV env) {
  switch (arg->type()) {
  case Type::symbol :
    return env->find_var(DPCS(arg));
  case Type::list : {
    PTR<List> lst = DPC<List>(arg);
    PTR<Funcs> f;
    switch (lst->car()->type()) {
    case Type::symbol : {
      PTR<Symbol> func_name = DPCS(lst->car());
      auto sf = find_special_func(func_name);
      if (sf)
        return sf(lst->cdr(), env);
      f = find_func(DPCS(lst->car()), env);
      break;
    }
    case Type::list : {
      PTR<Symbol> caar = DPCS(DPCL(lst->car())->car());
      if (caar && caar->get_value() == "LAMBDA") {
        f = DPC<Func>(lambda_(DPCL(lst->car())->cdr(), env));
        break;
      }
    }
    default :
      throw std::invalid_argument("Not calling a function");
    }
    std::vector<PTR<Sexp>> param;
    for (auto i = lst->cdr(); i && !i->nil(); i = i->cdr()) {
      if (param.size() >= f->get_ub())
        throw std::invalid_argument("Too many arguments");
      param.push_back(evaluate(i->car(), env));
    }
    return f->call(param, env);
  }
  case Type::null :
  case Type::integer :
  case Type::rational :
  case Type::float_ :
  case Type::complex :
  case Type::boolean :
    return arg;
  default :   // sexp, number, atom
    throw std::invalid_argument("Unexpected type");
  }
}

