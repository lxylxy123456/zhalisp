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

template <typename T>
class sptr {
 public:
  sptr();
  sptr(T* p);
  template <typename S>
  sptr(const sptr<S>& sp);
  sptr<T> operator=(const sptr<T>& rhs);
  T* operator->();
  const T* operator->() const;
  T* get() const;
  T& operator*();
  const T& operator*() const;
  operator bool() const;
  bool operator!();

 private:
  mutable T* ptr;

  template <class S>
  friend class sptr;
  template <typename S>
  friend sptr<T> sptr_cast(const sptr<S>&);
  template <typename S>
  friend sptr<T> sptr_cast(sptr<S>&&);
};

template <typename T, typename S>
sptr<T> sptr_cast(const sptr<S>& s);

/*
template <typename T, typename S>
sptr<T> sptr_cast(sptr<S>&& s) {
  return sptr<T>(dynamic_cast<T*>(s.get()));
}
*/
#endif
