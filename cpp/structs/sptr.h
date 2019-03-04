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

#ifndef SPTR_H
#define SPTR_H

#define PTR sptr
#define DPC sptr_cast

template <typename T>
class sptr {
 public:
  sptr() : ptr(nullptr) {}
  sptr(T* p) : ptr(p) {}
  template <typename S>
  sptr(const sptr<S>& sp) : ptr(sp.ptr) {}
  sptr<T> operator=(const sptr<T>& rhs) {
    ptr = rhs.ptr;
    return *this;
  }
  T* operator->() { return ptr; }
  const T* operator->() const { return ptr; }
  T* get() { return ptr; }
  const T* get() const { return ptr; }
  T& operator*() { return *ptr; }
  const T& operator*() const { return *ptr; }
  operator bool() const { return ptr; }
  bool operator!() { return !ptr; }
  
 private:
  T* ptr;

  template<class S>
  friend class sptr;
};

template <typename T, typename S>
sptr<T> sptr_cast(const sptr<S>& s) {
  // TODO: const is discarded
  // return sptr<T>(dynamic_cast<T*>(static_cast<S*>(s.get())));
  return sptr<T>();
}

#endif
