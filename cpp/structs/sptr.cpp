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

template<typename T>
sptr<T>::sptr() : ptr(nullptr) {}

template<typename T>
sptr<T>::sptr(T* p) : ptr(p) {}

template<typename T>
template<typename S>
sptr<T>::sptr(const sptr<S>& sp) : ptr(sp.ptr) {}

template<typename T>
sptr<T> sptr<T>::operator=(const sptr<T>& rhs) {
  ptr = rhs.ptr;
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

// Explicit template instantiation
template class sptr<Bool>;
template class sptr<Complex>;
template class sptr<Env>;
template class sptr<Float>;
template class sptr<Funcs>;
template class sptr<Func>;
template class sptr<EFunc>;
template class sptr<CadrFunc>;
template class sptr<Integer>;
template class sptr<List>;
template class sptr<Nil>;
template class sptr<Number>;
template class sptr<Rational>;
template class sptr<Sexp>;
template class sptr<Symbol>;

