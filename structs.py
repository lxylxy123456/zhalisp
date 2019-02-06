# 
# zhalisp - A "zha" Clisp implementation
# Copyright (C) 2019  lxylxy123456
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# 

from fractions import Fraction

class Atom :
	def __repr__(self) :
		raise NotImplementedError

class Dot :
	def __str__(self) :
		return '.'
	def __repr__(self) :
		return 'DOT<.>'

class Number(Atom) :
	def __init__(self, value) :
		# assert type(value) in (int, float, Fraction, complex)
		if type(value) == complex :
			raise NotImplementedError('complex numbers')
		self.value = value
	def __str__(self) :
		return str(self.value)
	def __repr__(self) :
		return 'Number<%s>' % repr(self.value)
	def type(self) :
		return type(self.value)
	def __add__(self, rhs) :
		return Number(self.value + rhs.value)
	def __sub__(self, rhs) :
		return Number(self.value - rhs.value)
	def __neg__(self) :
		return Number(-self.value)
	def __mul__(self, rhs) :
		return Number(self.value * rhs.value)
	def __truediv__(self, rhs) :
		if (self.type() == int and rhs.type() == int) :
			return Number(Fraction(self.value, rhs.value))
		else :
			return Number(self.value / rhs.value)
	def equal(self, rhs) :
		return self.value == rhs.value
	def __lt__(self, rhs) :
		return self.value < rhs.value
	def __gt__(self, rhs) :
		return self.value > rhs.value
	def __le__(self, rhs) :
		return self.value <= rhs.value
	def __ge__(self, rhs) :
		return self.value >= rhs.value

class Symbol(Atom) :
	def __init__(self, value) :
		# assert type(value) == str
		self.value = value.upper()
	def __str__(self) :
		return self.value
	def __repr__(self) :
		return 'Symbol<%s>' % repr(self.value)

class Bool(Atom) :
	'The T value for True'
	def __str__(self) :
		return 'T'
	def __repr__(self) :
		return 'Bool<T>'

class List :
	def __init__(self, car=None, cdr=None) :
		'List is nil iff cdr == None; car is meaningful iff cdr != None'
		assert not (cdr == None and car != None)
		assert type(car) in (Env, Atom, Number, Symbol, List, Bool, type(None))
		assert type(cdr) in (Env, Atom, Number, Symbol, List, Bool, type(None))
		self.car = car
		self.cdr = cdr
	def nil(self) :
		return self.cdr == None
	def get_car(self) :
		if self.nil() :
			return self
		else :
			return self.car
	def get_cdr(self) :
		if self.nil() :
			return self
		else :
			return self.cdr
	def __iter__(self) :
		s = self
		while s :
			yield s.car
			if type(s.cdr) != List :
				yield Dot()
				yield s.cdr
				break
			elif not s.cdr.nil() :
				s = s.cdr
			else :
				break
	def to_list(self) :
		ans = []
		for i in self :
			if type(i) == List :
				ans.append(i.to_list())
			else :
				ans.append(i)
		return ans
	def __str__(self) :
		'Return a s-expression list that should be interpreted by lisp'
		if self.nil() :
			return 'NIL'
		ans = '('
		s = self
		while s :
			ans += s.car.__str__()
			if type(s.cdr) != List :
				ans += ' ' + str(Dot()) + ' ' + str(s.cdr)
				break
			elif s.cdr.nil() :
				break
			else :
				s = s.cdr
				ans += ' '
		return ans + ')'
	def __repr__(self) :
		return 'List<' + str(self) + '>'

class Env :
	def __init__(self) :
		self.variable = {}
		self.function = {}
	def __repr__(self) :
		return 'Env<var=%s, fun=%s>' % (self.variable, self.function)
	def has_var(self, k) :
		assert type(k) == str
		return self.variable.__contains__(k)
	def get_var(self, k, default=None) :
		assert type(k) == str
		return self.variable.get(k, default)
	def set_var(self, k, v) :
		assert type(k) == str
		self.variable.__setitem__(k, v)
	def has_fun(self, k) :
		assert type(k) == str
		return self.function.__contains__(k)
	def get_fun(self, k, default=None) :
		assert type(k) == str
		return self.function.get(k, default)
	def set_fun(self, k, v) :
		assert type(k) == str
		self.function.__setitem__(k, v)
