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

#include "sptr.h"

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
#include "sptr.h"
#include "symbol.h"

#ifdef DEBUG_SPTR
  #include <iostream>
#endif

template<typename T>
sptr<T>::sptr() : ptr(nullptr), use_cnt(nullptr) {
#ifdef DEBUG_SPTR
  std::cout << "C1 " << this << std::endl;
#endif
}

template<typename T>
sptr<T>::sptr(T* p) : ptr(p), use_cnt(ptr ? new int(1) : nullptr) {
#ifdef DEBUG_SPTR
  std::cout << "C2 " << this << std::endl;
#endif
}

template<typename T>
sptr<T>::sptr(const sptr<T>& sp) : ptr(sp.ptr), use_cnt(sp.use_cnt) {
#ifdef DEBUG_SPTR
  std::cout << "C3 " << this << std::endl;
#endif
  if (ptr)
    (*use_cnt)++;
}

template<typename T>
sptr<T>::sptr(sptr<T>&& sp) : ptr(sp.ptr), use_cnt(sp.use_cnt) {
#ifdef DEBUG_SPTR
  std::cout << "C4 " << this << std::endl;
#endif
  if (ptr)
    (*use_cnt)++;   // 0/0: may not be necessary
}

template<typename T>
template<typename S>
sptr<T>::sptr(const sptr<S>& sp) : ptr(sp.ptr), use_cnt(sp.use_cnt) {
#ifdef DEBUG_SPTR
  std::cout << "C5 " << this << std::endl;
#endif
  if (ptr)
    (*use_cnt)++;
}

template<typename T>
sptr<T>::sptr(T* p, int* u) : ptr(p), use_cnt(u) {
#ifdef DEBUG_SPTR
  std::cout << "C6 " << this << std::endl;
#endif
  if (ptr)
    (*use_cnt)++;
}

template <typename T>
sptr<T>::~sptr() {
  if (ptr) {
    (*use_cnt)--;
#ifdef DEBUG_SPTR
    std::cout << "d  " << this << "\t" << *use_cnt << std::endl;  // 0/0
#endif
    if (!*use_cnt) {
#ifdef DEBUG_SPTR
      std::cout << "~  " << this << std::endl;  // 0/0
#endif
      delete ptr;
      delete use_cnt;
    }
  }
}

template<typename T>
sptr<T>& sptr<T>::operator=(const sptr<T>& rhs) {
  this->~sptr();
  ptr = rhs.ptr;
  use_cnt = rhs.use_cnt;
  if (ptr)
    (*use_cnt)++;
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

template <typename T, typename S>
sptr<T> sptr_cast(const sptr<S>& s) {
  T* cast = dynamic_cast<T*>(s.ptr);
  if (cast)
    return sptr<T>(cast, s.use_cnt);
  else
    return sptr<T>();
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

