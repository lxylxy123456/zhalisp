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

// Allow tail-recursion optimization: #define TAIL_RECU
// Allow check for stack overflow:    #define LIMIT_STACK <eval stack size>

// Conversion

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

PTR<Funcs> FDPCFuncs(PTR<Sexp> p) {
  PTR<Funcs>&& ans = DPC<Funcs>(p);
  if (!ans)
    throw std::invalid_argument("Not a function");
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
  PTR<Symbol> sym = DPCS(s);
  if (sym)
    return find_func(sym, env);
  else
    return FDPCFuncs(s);
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

#ifdef LIMIT_STACK
char* bottom_stack = nullptr;
size_t stack_limit = (LIMIT_STACK);

void check_stack() {
  char* top_stack = reinterpret_cast<char*>(&top_stack);
  if (stack_limit && bottom_stack > top_stack &&
      (size_t) (bottom_stack - top_stack) > stack_limit)
    throw std::invalid_argument("Stack overflow");
}
#endif

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
  ARGS_SIZE_LB(1)
  PTR<Number> n = FDPCN(args[0]);
  for (auto i = args.begin() + 1; i != args.end(); i++)
    if (!((*n) == (*FDPCN(*i))))
      return BOOL(false);
  return BOOL(true);
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
    l = l->fcdr();
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
      if (!i)
        throw std::invalid_argument("Dotted list");
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
      if (!i)
        throw std::invalid_argument("Dotted list");
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
      if (!i)
        throw std::invalid_argument("Dotted list");
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
  if (args->fcdr()->nil())
    throw std::invalid_argument("Too few arguments");
  PTR<Symbol> f_name = FDPCS(args->car());
  if (reserved_func(f_name))
    throw std::invalid_argument("Reserved function name");
  std::vector<PTR<Symbol>> f_args;
  for (PTR<List> i = FDPCL(args->fcdr()->car()); !i->nil();
        i = FDPCL(i->r_cdr()))
    f_args.push_back(FDPCS(i->car()));
  PTR<List> f_stmt = args->fcdr()->fcdr();
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
  PTR<List> f_stmt = args->fcdr();
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
  for (PTR<List> j = last_list; !j->nil(); j = j->fcdr())
    arg.push_back(j->car());
  return func->call(arg, env);
}

#ifdef TAIL_RECU
TRInfo apply_recu(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_LB(2)
  PTR<Funcs> func = sym_to_func(args[0], env);
  if (!func)
    throw std::invalid_argument("Not a function");
  std::vector<PTR<Sexp>>* arg = new std::vector<PTR<Sexp>>();
  auto i = args.begin();
  for (i++; i + 1 != args.end(); i++)
    arg->push_back(*i);
  PTR<List> last_list = FDPCL(*i);
  for (PTR<List> j = last_list; !j->nil(); j = j->fcdr())
    arg->push_back(j->car());
  return TRInfo(func, arg, env);
}
#else
TRInfo(*apply_recu)(const std::vector<PTR<Sexp>>& args, ENV env) = nullptr;
#endif

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

#ifdef TAIL_RECU
TRInfo funcall_recu(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_LB(1)
  PTR<Funcs> func = sym_to_func(args[0], env);
  if (!func)
    throw std::invalid_argument("Not a function");
  std::vector<PTR<Sexp>>* arg = new std::vector<PTR<Sexp>>();
  auto i = args.begin();
  for (i++; i != args.end(); i++)
    arg->push_back(*i);
  return TRInfo(func, arg, env);
}
#else
TRInfo(*funcall_recu)(const std::vector<PTR<Sexp>>& args, ENV env) = nullptr;
#endif

PTR<Sexp> function(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->fcdr()->nil())
    throw std::invalid_argument("Too many arguments");
  PTR<Symbol> as = DPCS(args->car());
  if (as) {
    return find_func(as, env);
  } else {
    PTR<List> lst = FDPCL(args->car());
    assert(FDPCS(lst->car())->get_value() == "LAMBDA");
    return lambda_(lst->fcdr(), env);
  }
}

PTR<Sexp> quote(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->fcdr()->nil())
    throw std::invalid_argument("Too many arguments");
  return args->car();
}

PTR<Sexp> eval_(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return evaluate(args[0], env);
}

#ifdef TAIL_RECU
TRInfo eval__recu(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
  return TRInfo(args[0], env);
}
#else
TRInfo(*eval__recu)(const std::vector<PTR<Sexp>>& args, ENV env) = nullptr;
#endif

// Variables

PTR<Sexp> let(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  ENV new_env(new Env{env, "LET"});
  for (PTR<List> i = DPCL(args->car()); i && !i->nil(); i = i->cdr()) {
    PTR<Symbol> k = DPCS(i->car());
    PTR<Sexp> v = Nil::lisp_nil;
    if (!k) {
      PTR<List> il = FDPCL(i->car());
      if (!il->fcdr()->fcdr()->nil())
        throw std::invalid_argument("Argument too long");
      k = FDPCS(il->car());
      v = evaluate(il->fcdr()->car(), env);
    }
    new_env->set_var(k, v);
  }
  PTR<Sexp> ans = Nil::lisp_nil;
  for (PTR<List> i = args->cdr(); i && !i->nil(); i = i->cdr())
    ans = evaluate(i->car(), new_env);
  return ans;
}

#ifdef TAIL_RECU
TRInfo let_recu(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  ENV new_env(new Env{env, "LET"});
  for (PTR<List> i = DPCL(args->car()); i && !i->nil(); i = i->cdr()) {
    PTR<Symbol> k = DPCS(i->car());
    PTR<Sexp> v = Nil::lisp_nil;
    if (!k) {
      PTR<List> il = FDPCL(i->car());
      if (!il->fcdr()->fcdr()->nil())
        throw std::invalid_argument("Argument too long");
      k = FDPCS(il->car());
      v = evaluate(il->fcdr()->car(), env);
    }
    new_env->set_var(k, v);
  }
  if (!args->cdr() || args->cdr()->nil())
    return TRInfo(Nil::lisp_nil);
  PTR<List> i = args->cdr();
  for (; i->cdr() && !i->cdr()->nil(); i = i->cdr())
    evaluate(i->car(), new_env);
  return TRInfo(i->car(), new_env);
}
#endif

PTR<Sexp> let_star(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  ENV new_env(new Env{env, "LET*"});
  for (PTR<List> i = DPCL(args->car()); i && !i->nil(); i = i->cdr()) {
    PTR<Symbol> k = DPCS(i->car());
    PTR<Sexp> v = Nil::lisp_nil;
    if (!k) {
      PTR<List> il = FDPCL(i->car());
      if (!il->fcdr()->fcdr()->nil())
        throw std::invalid_argument("Argument too long");
      k = FDPCS(il->car());
      v = evaluate(il->fcdr()->car(), new_env);
    }
    new_env->set_var(k, v);
  }
  PTR<Sexp> ans = Nil::lisp_nil;
  for (PTR<List> i = args->cdr(); i && !i->nil(); i = i->cdr())
    ans = evaluate(i->car(), new_env);
  return ans;
}

#ifdef TAIL_RECU
TRInfo let_star_recu(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  ENV new_env(new Env{env, "LET*"});
  for (PTR<List> i = DPCL(args->car()); i && !i->nil(); i = i->cdr()) {
    PTR<Symbol> k = DPCS(i->car());
    PTR<Sexp> v = Nil::lisp_nil;
    if (!k) {
      PTR<List> il = FDPCL(i->car());
      if (!il->fcdr()->fcdr()->nil())
        throw std::invalid_argument("Argument too long");
      k = FDPCS(il->car());
      v = evaluate(il->fcdr()->car(), new_env);
    }
    new_env->set_var(k, v);
  }
  if (!args->cdr() || args->cdr()->nil())
    return TRInfo(Nil::lisp_nil);
  PTR<List> i = args->cdr();
  for (; i->cdr() && !i->cdr()->nil(); i = i->cdr())
    evaluate(i->car(), new_env);
  return TRInfo(i->car(), new_env);
}
#endif

PTR<Sexp> setq(PTR<List> args, ENV env) {
  int arg_count = 0;
  for (PTR<List> i = args; !i->nil(); i = i->fcdr())
    arg_count++;
  if (arg_count % 2)
    throw std::invalid_argument("Odd number of arguments");
  PTR<Sexp> v = Nil::lisp_nil;
  for (PTR<List> i = args; !i->nil(); i = i->cdr()) {
    PTR<Symbol> k = FDPCS(i->car());
    i = i->cdr();
    v = evaluate(i->car(), env);
    env->set_vars(k, v);
  }
  return v;
}

PTR<Sexp> set(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(2)
  env->set_vars(FDPCS(args[0]), args[1]);
  return args[1];
}

// Conditions

PTR<Sexp> cond(PTR<List> args, ENV env) {
  for (PTR<List> i = args; i && !i->nil(); i = i->cdr()) {
    PTR<List> test = FDPCL(i->car());
    if (test->nil())
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

#ifdef TAIL_RECU
TRInfo cond_recu(PTR<List> args, ENV env) {
  for (PTR<List> i = args; i && !i->nil(); i = i->cdr()) {
    PTR<List> test = FDPCL(i->car());
    if (test->nil())
      throw std::invalid_argument("Should pass a list");
    PTR<Sexp> ans = evaluate(test->car(), env);
    if (ans->t()) {
      PTR<List> j = test->cdr();
      if (!j || j->nil())
        return TRInfo(ans);
      for (; j->cdr() && !j->cdr()->nil(); j = j->cdr())
        evaluate(j->car(), env);
      return TRInfo(j->car(), env);
    }
  }
  return TRInfo(Nil::lisp_nil);
}
#endif

PTR<Sexp> if_(PTR<List> args, ENV env) {
  if (args->fcdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->fcdr()->fcdr()->fcdr()->nil())
    throw std::invalid_argument("Too many arguments");
  if (evaluate(args->car(), env)->t())
    return evaluate(args->fcdr()->car(), env);
  else
    return evaluate(args->fcdr()->fcdr()->car(), env);
}

#ifdef TAIL_RECU
TRInfo if__recu(PTR<List> args, ENV env) {
  if (args->fcdr()->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->fcdr()->fcdr()->fcdr()->nil())
    throw std::invalid_argument("Too many arguments");
  if (evaluate(args->car(), env)->t())
    return TRInfo(args->fcdr()->car(), env);
  else
    return TRInfo(args->fcdr()->fcdr()->car(), env);
}
#endif

// Iteration

PTR<Sexp> do_(PTR<List> args, ENV env) {
  if (args->fcdr()->nil())
    throw std::invalid_argument("Too few arguments");
  ENV new_env(new Env{env, "DO"});
  std::vector<std::pair<PTR<Symbol>, PTR<Sexp>>> val_list;
  for (PTR<List> i = DPCL(args->car()); !i->nil(); i = i->fcdr()) {
    PTR<List> li = DPCL(i->car());
    if (li) {
      assert(li->fcdr()->fcdr()->fcdr()->nil());
      PTR<Symbol> name = FDPCS(li->car());
      new_env->set_var(name, evaluate(li->fcdr()->car(), env));
      if (!li->fcdr()->fcdr()->nil())
        val_list.emplace_back(name, li->fcdr()->fcdr()->car());
    } else {
      new_env->set_var(FDPCS(i->car()), Nil::lisp_nil);
    }
  }
  PTR<Sexp> exit_test = FDPCL(args->fcdr()->car())->car();
  while (evaluate(exit_test, new_env)->nil()) {
    for (PTR<List> i = args->fcdr()->fcdr(); !i->nil(); i = i->fcdr())
      evaluate(i->car(), new_env);
    std::vector<std::pair<PTR<Symbol>, PTR<Sexp>>> new_values;
    for (auto i = val_list.begin(); i != val_list.end(); i++)
      new_values.emplace_back(i->first, evaluate(i->second, new_env));
    for (auto i = new_values.begin(); i != new_values.end(); i++)
      new_env->set_var(i->first, i->second);
  }
  PTR<Sexp> ans = Nil::lisp_nil;
  for (PTR<List> i = FDPCL(args->fcdr()->car())->fcdr(); !i->nil();
        i = i->fcdr())
    ans = evaluate(i->car(), new_env);
  return ans;
}

class ProgInterrupt: public std::exception {
 public:
  ProgInterrupt(int t, ENV e, PTR<Sexp> v): type(t), env(e), value(v) {}
  int type;       // 1: go; 2: return
  PTR<Env> env;
  PTR<Sexp> value;
  virtual const char* what() const noexcept { return "ProgInterrupt"; }
};

PTR<Sexp> prog(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  ENV new_env(new Env{env, "PROG"});
  for (PTR<List> i = DPCL(args->car()); !i->nil(); i = i->fcdr()) {
    PTR<List> li = DPCL(i->car());
    if (li) {
      assert(li->fcdr()->fcdr()->nil());
      new_env->set_var(FDPCS(li->car()), evaluate(li->fcdr()->car(), env));
    } else {
      new_env->set_var(FDPCS(i->car()), Nil::lisp_nil);
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
      evaluate(sequence[cur], new_env);
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
  if (!args->fcdr()->nil())
    throw std::invalid_argument("Too many arguments");
  throw ProgInterrupt{1, env, args->car()};
}

PTR<Sexp> return_(PTR<List> args, ENV env) {
  if (args->nil())
    throw std::invalid_argument("Too few arguments");
  if (!args->fcdr()->nil())
    throw std::invalid_argument("Too many arguments");
  throw ProgInterrupt{2, env, evaluate(args->car(), env)};
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

PTR<Sexp> setstacklimit(const std::vector<PTR<Sexp>>& args, ENV env) {
  ARGS_SIZE_EQ(1)
#ifdef LIMIT_STACK
  const mpz_class& val = FDPCI(args[0])->get_value();
  if (val < 0 || val >= std::numeric_limits<std::size_t>::max())
    throw std::invalid_argument("Invalid stack size");
  stack_limit = val.get_ui();
#endif
  return args[0];
}

// Evaluate

#define REGISTER_EFUNC(K, ...) {K, PTR<EFunc>(new EFunc(K, __VA_ARGS__))},

std::unordered_map<std::string, PTR<EFunc>> fmap = {
  REGISTER_EFUNC("+", plus, nullptr)
  REGISTER_EFUNC("-", minus, nullptr, 1)
  REGISTER_EFUNC("*", mul, nullptr)
  REGISTER_EFUNC("/", div, nullptr, 1)
  REGISTER_EFUNC("1+", one_plus, nullptr, 1, 1)
  REGISTER_EFUNC("1-", one_minus, nullptr, 1, 1)
  REGISTER_EFUNC("=", eq_, nullptr, 1)
  REGISTER_EFUNC("<", lt, nullptr, 2, 2)
  REGISTER_EFUNC("<=", le, nullptr, 2, 2)
  REGISTER_EFUNC(">", gt, nullptr, 2, 2)
  REGISTER_EFUNC(">=", ge, nullptr, 2, 2)
  REGISTER_EFUNC("SQRT", sqrt_, nullptr, 1, 1)
  REGISTER_EFUNC("ATOM", atom, nullptr, 1, 1)
  REGISTER_EFUNC("LISTP", listp, nullptr, 1, 1)
  REGISTER_EFUNC("NULL", null, nullptr, 1, 1)
  REGISTER_EFUNC("NUMBERP", numberp, nullptr, 1, 1)
  REGISTER_EFUNC("TYPEP", typep, nullptr, 2, 2)
  REGISTER_EFUNC("SYMBOLP", symbolp, nullptr, 1, 1)
  REGISTER_EFUNC("ZEROP", zerop, nullptr, 1, 1)
  REGISTER_EFUNC("EVENP", evenp, nullptr, 1, 1)
  REGISTER_EFUNC("ODDP", oddp, nullptr, 1, 1)
  REGISTER_EFUNC("EQ", eq, nullptr, 2, 2)
  REGISTER_EFUNC("EQL", eql, nullptr, 2, 2)
  REGISTER_EFUNC("EQUAL", equal, nullptr, 2, 2)
  REGISTER_EFUNC("NOT", not_, nullptr, 1, 1)
  REGISTER_EFUNC("CAR", car, nullptr, 1, 1)
  REGISTER_EFUNC("CDR", cdr, nullptr, 1, 1)
  REGISTER_EFUNC("CONS", cons, nullptr, 2, 2)
  REGISTER_EFUNC("LIST", list_, nullptr)
  REGISTER_EFUNC("MEMBER", member, nullptr, 2, 2)
  REGISTER_EFUNC("MAPCAR", mapcar, nullptr, 2)
  REGISTER_EFUNC("MAPC", mapc, nullptr, 2)
  REGISTER_EFUNC("MAPLIST", maplist, nullptr, 2)
  REGISTER_EFUNC("APPEND", append, nullptr)
  REGISTER_EFUNC("APPLY", apply, apply_recu, 2)
  REGISTER_EFUNC("FUNCALL", funcall, funcall_recu, 1)
  REGISTER_EFUNC("EVAL", eval_, eval__recu, 1, 1)
  REGISTER_EFUNC("SET", set, nullptr, 2, 2)
  REGISTER_EFUNC("PRINT", print_, nullptr, 1, 1)
  REGISTER_EFUNC("SETRECURSIONLIMIT", setrecursionlimit, nullptr, 1, 1)
  REGISTER_EFUNC("SETSTACKLIMIT", setstacklimit, nullptr, 1, 1)
};

#define CFUNC_TYPE(N) PTR<Sexp>(*N)(PTR<List>, ENV)

std::unordered_map<std::string, CFUNC_TYPE()> special_funcs = {
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

#ifdef TAIL_RECU
#define CFUNCTRO_TYPE(N) TRInfo(*N)(PTR<List>, ENV)

std::unordered_map<std::string, CFUNCTRO_TYPE()> special_funcs_tro = {
  {"LET", let_recu},
  {"LET*", let_star_recu},
  {"COND", cond_recu},
  {"IF", if__recu},
};
#endif

bool reserved_func(PTR<Symbol> sym) {
  const std::string& fun_name = sym->get_value();
  if (special_funcs.find(fun_name) != special_funcs.end())
    return true;
  if (fmap.find(fun_name) != fmap.end())
    return true;
  static const std::regex re_caordr("C[AD]{1,4}R");
  if (std::regex_match(fun_name, re_caordr))
    return true;
  return false;
}

CFUNC_TYPE(find_special_func(PTR<Symbol> sym)) {
  auto found = special_funcs.find(sym->get_value());
  if (found == special_funcs.end())
    return nullptr;
  else
    return found->second;
}

PTR<Funcs> find_func(PTR<Symbol> sym, ENV env) {
  const std::string& fun_name = sym->get_value();
  auto found = fmap.find(fun_name);
  if (found != fmap.end())
    return found->second;
  static const std::regex re_caordr("C[AD]{1,4}R");
  if (std::regex_match(fun_name, re_caordr))
    return PTR<CadrFunc>(new CadrFunc(fun_name, caordr));
  PTR<Funcs> func = env->find_fun(sym);
  return func;
}

PTR<Sexp> evaluate(PTR<Sexp> arg, ENV env) {
#ifdef LIMIT_STACK
  if (!bottom_stack) {
    char v;
    bottom_stack = &v;
  }
#endif
#ifdef TAIL_RECU
  TRInfo trinfo;
start_eval:
#endif
  switch (arg->type()) {
  case Type::symbol :
    return env->find_var(DPCS(arg));
  case Type::list : {
    PTR<List> lst = DPCL(arg);
    PTR<Funcs> f;
    switch (lst->car()->type()) {
    case Type::symbol : {
      PTR<Symbol> func_name = DPCS(lst->car());
      auto sf = find_special_func(func_name);
      if (sf) {
#ifdef TAIL_RECU
        auto found_tro = special_funcs_tro.find(func_name->get_value());
        if (found_tro != special_funcs_tro.end()) {
          trinfo = found_tro->second(lst->fcdr(), env);
          goto proc_trinfo;
        }
#endif
        return sf(lst->fcdr(), env);
      }
      f = find_func(func_name, env);
      break;
    }
    case Type::list : {
      PTR<List> l_exp = DPCL(lst->car());
      PTR<Symbol> caar = FDPCS(l_exp->car());
      if (caar->get_value() == "LAMBDA") {
        f = DPCFuncs(lambda_(l_exp->fcdr(), env));
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
#ifdef LIMIT_STACK
    check_stack();
#endif
#ifdef TAIL_RECU
    if (f->has_tro()) {
      trinfo = f->call_tro(param, env);
      goto proc_trinfo;
    }
#endif
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
#ifdef TAIL_RECU
proc_trinfo:
  if (!trinfo.env) {
    return trinfo.sexp;
  } else if (!trinfo.args) {
    arg = trinfo.sexp;
    env = trinfo.env;
    goto start_eval;
  } else {
    PTR<Funcs> f = DPCFuncs(trinfo.sexp);
#ifdef LIMIT_STACK
    check_stack();
#endif
    if (f->has_tro()) {
      trinfo = f->call_tro(*trinfo.args, trinfo.env);
      goto proc_trinfo;
    }
    return f->call(*trinfo.args, trinfo.env);
  }
#endif
}

