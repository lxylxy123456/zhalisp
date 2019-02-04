from fractions import Fraction

class Atom :
	def __repr__(self) :
		raise NotImplementedError

class Dot :
	def __str__(self) :
		return '.'
	def __repr__(self) :
		return 'DOT()'

class Number(Atom) :
	def __init__(self, value) :
		# assert type(value) in (int, float, Fraction, complex)
		if type(value) == complex :
			raise NotImplementedError('complex numbers')
		self.value = value
	def __str__(self) :
		return str(self.value)
	def __repr__(self) :
		return repr(self.value)
	def type(self) :
		return type(self.value)
	def __add__(self, rhs) :
		return Number(self.value + rhs.value)
	def __sub__(self, rhs) :
		return Number(self.value - rhs.value)
	def __mul__(self, rhs) :
		return Number(self.value * rhs.value)
	def __truediv__(self, rhs) :
		if (self.type() == int and rhs.type() == int) :
			return Number(Fraction(self.value, rhs.value))
		else :
			return Number(self.value / rhs.value)

class Symbol(Atom) :
	def __init__(self, value) :
		# assert type(value) == str
		self.value = value
	def __str__(self) :
		return self.value
	def __repr__(self) :
		return repr(self.value)

class List :
	def __init__(self, car=None, cdr=None) :
		'List is nil iff cdr == None; car is meaningful iff cdr != None'
		self.car = car
		self.cdr = cdr
	def nil(self) :
		return self.cdr == None
	def to_list(self, recurse=False) :
		ans = []
		s = self
		while s :
			if recurse and type(s.car) == List :
				ans.append(s.car.to_list(recurse))
			else :
				ans.append(s.car)
			if type(s.cdr) != List :
				ans.append(Dot)
				ans.append(s.cdr)
				break
			elif not s.cdr.nil() :
				s = s.cdr
			else :
				break
		return ans
	def __str__(self) :
		'Return a s-expression list that should be interpreted by lisp'
		ans = '('
		s = self
		while s :
			if type(s.car) == List :
				ans += s.car.__str__()
			else :
				ans += str(s.car)
			if s.cdr == None :
				break
			elif type(s.cdr) != List :
				ans += ' ' + str(Dot()) + ' ' + str(s.cdr)
				break
			elif s.cdr.nil() :
				break
			else :
				s = s.cdr
				ans += ' '
		return ans + ')'

class Env :
	def __init__(self) :
		self.variable = {}
		self.function = {}
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

