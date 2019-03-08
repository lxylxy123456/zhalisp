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

#ifdef CUSTOM_PTR

#include "sptr.h"

#include <cassert>

#include "bool.h"
#include "complex.h"
#include "environment.h"
#include "float.h"
#include "func.h"
#include "integer.h"
#include "list.h"
#include "nil.h"
#include "number.h"
#include "rational.h"
#include "sexp.h"
#include "symbol.h"

// Enable sptr:                       #define CUSTOM_PTR
// Print debug information:           #define DEBUG_SPTR

#ifdef DEBUG_SPTR
  #include <iostream>
#endif

std::unordered_map<void*, int*> sptr_items;

template <typename T>
int get_dynamic_type(T* p) {
  static_assert(std::is_base_of<Sexp, T>::value, "");
  return p ? static_cast<int>(p->type()) : -3;
}

int get_dynamic_type(Env* p) {
  return p ? -1 : -3;
}

int get_dynamic_type(std::vector<sptr<Sexp>>* p) {
  return p ? -2 : -3;
}

template<typename T>
sptr<T>::sptr() : ptr(nullptr), use_cnt(nullptr), type(-3) {
#ifdef DEBUG_SPTR_
  std::cout << "c " << this << std::endl;
#endif
}

template<typename T>
sptr<T>::sptr(T* p) : sptr<T>(p, p ? new int(0) : nullptr,
                              get_dynamic_type(p)) {}

template<typename T>
sptr<T>::sptr(const sptr<T>& sp) : sptr<T>(sp.ptr, sp.use_cnt, sp.type) {}

template<typename T>
sptr<T>::sptr(sptr<T>&& sp) : sptr<T>(sp.ptr, sp.use_cnt, sp.type) {}

template<typename T>
template<typename S>
sptr<T>::sptr(const sptr<S>& sp) : sptr<T>(sp.ptr, sp.use_cnt, sp.type) {}

template<typename T>
sptr<T>::sptr(T* p, int* u, int t) : ptr(p), use_cnt(u), type(t) {
#ifdef DEBUG_SPTR_
  std::cout << "C " << this << std::endl;
#endif
  this->inc_use();
}

template <typename T>
sptr<T>::~sptr() {
  this->dec_use();
}

template<typename T>
sptr<T>& sptr<T>::operator=(const sptr<T>& rhs) {
  this->dec_use();
  ptr = rhs.ptr;
  use_cnt = rhs.use_cnt;
  type = rhs.type;
  this->inc_use();
  return *this;
}

template<typename T>
T* sptr<T>::operator->() { return ptr; }

template<typename T>
const T* sptr<T>::operator->() const { return ptr; }

template<typename T>
T* sptr<T>::get() const { return ptr; }

template<typename T>
T& sptr<T>::operator*() { return *ptr; }

template<typename T>
const T& sptr<T>::operator*() const { return *ptr; }

template<typename T>
sptr<T>::operator bool() const { return ptr; }

template<typename T>
bool sptr<T>::operator!() { return !ptr; }

template<typename T>
inline void sptr<T>::dec_use() {
  if (ptr) {
    (*use_cnt)--;
#ifdef DEBUG_SPTR
    std::cout << "D " << this << "\t" << *use_cnt << (*use_cnt ? "\t" : "\t~")
              << std::endl;
#endif
    if (!*use_cnt) {
      if (std::is_convertible<T, Env>::value) {
        sptr_items.erase(ptr);
      }
      delete ptr;
      delete use_cnt;
    }
  }
}

template<typename T>
inline void sptr<T>::inc_use() {
  if (ptr) {
    (*use_cnt)++;
    if (std::is_convertible<T, Env>::value) {
      auto found = sptr_items.find(ptr);
      if (found != sptr_items.end())
        assert(found->second == use_cnt);
      else
        sptr_items[ptr] = use_cnt;
    }
  }
}

template <typename T, typename S>
sptr<T> sptr_cast(const sptr<S>& s) {
  T* cast = dynamic_cast<T*>(s.ptr);
  if (cast)
    return sptr<T>(cast, s.use_cnt, s.type);
  else
    return sptr<T>();
}

template <typename T>
void sweep_dfs(const sptr<T>& elem, std::unordered_set<void*>& visited) {
  if (visited.find(elem.ptr) != visited.end())
    return;
  switch (elem.type) {
    case -1: {                // Env
      visited.insert(elem.ptr);
      Env& e = *reinterpret_cast<Env*>(elem.ptr);
      for (auto i = e.variable.begin(); i != e.variable.end(); i++)
        sweep_dfs(i->second, visited);
      for (auto i = e.function.begin(); i != e.function.end(); i++)
        sweep_dfs(i->second, visited);
      sweep_dfs(e.outer, visited);
      break;
    }
    case static_cast<int>(Type::func): {
      visited.insert(elem.ptr);
      Func& e = *reinterpret_cast<Func*>(elem.ptr);
      sweep_dfs(e.f_stmt, visited);
      sweep_dfs(e.f_env, visited);
      break;
    }
    case static_cast<int>(Type::list): {
      visited.insert(elem.ptr);
      List& e = *reinterpret_cast<List*>(elem.ptr);
      sweep_dfs(e.l_car, visited);
      sweep_dfs(e.l_cdr, visited);
      break;
    }
    default:
      break;
  }
}

template <typename T>
void sptr_sweep(const sptr<T>& root) {
  std::unordered_set<void*> visited;
  if (root.ptr)
    sweep_dfs(root, visited);
  std::unordered_set<void*> remove_list;
  for (auto i = sptr_items.begin(); i != sptr_items.end(); i++)
    if (visited.find(i->first) == visited.end())
      remove_list.insert(i->first);
  for (auto i : remove_list) {
    auto found = sptr_items.find(i);
    if (found != sptr_items.end()) {
      sptr<Env> e = sptr<Env>(reinterpret_cast<Env*>(i), found->second, -1);
      e->outer = nullptr;
      e->variable.clear();
      e->function.clear();
    }
  }
}

// Explicit template instantiation
template class sptr<Sexp>;
template class sptr<Symbol>;
template class sptr<Number>;
template class sptr<Integer>;
template class sptr<Rational>;
template class sptr<Float>;
template class sptr<Complex>;
template class sptr<Bool>;
template class sptr<List>;
template class sptr<Nil>;
template class sptr<Funcs>;
template class sptr<Func>;
template class sptr<EFunc>;
template class sptr<CadrFunc>;
template class sptr<Env>;
template class sptr<std::vector<sptr<Sexp>, std::allocator<sptr<Sexp>>>>;

#define INSTANTIATE(S, T)\
  template sptr<S>::sptr<T>(const sptr<T>&); \
  template sptr<T> sptr_cast<T, S>(const sptr<S>& s);

INSTANTIATE(Sexp, Symbol);
INSTANTIATE(Sexp, Number);
INSTANTIATE(Sexp, Integer);
INSTANTIATE(Sexp, Rational);
INSTANTIATE(Sexp, Float);
INSTANTIATE(Sexp, Complex);
INSTANTIATE(Sexp, Bool);
INSTANTIATE(Sexp, List);
INSTANTIATE(Sexp, Nil);
INSTANTIATE(Sexp, Func);
INSTANTIATE(Sexp, Funcs);
INSTANTIATE(Number, Integer);
INSTANTIATE(Number, Rational);
INSTANTIATE(Number, Float);
INSTANTIATE(Number, Complex);
INSTANTIATE(List, Nil);
INSTANTIATE(Funcs, Func);
INSTANTIATE(Funcs, EFunc);
INSTANTIATE(Funcs, CadrFunc);

template void sptr_sweep<Env>(const sptr<Env>& root);

#endif  // CUSTOM_PTR
