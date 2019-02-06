'''
	builtin functions in lisp
	LIMIT: complex + fraction will have different behavior than Clisp
	LIMIT: eq have different behavior than Clisp (currently same as eql)
'''

import functools, operator, re

from structs import Env, Atom, Number, Symbol, List, Bool, Dot
from itertools import repeat

# Helper functions

def eval_params(exps, env) :
	'((+ 1 2) (- 3 4)) -> [3, -1]'
	return tuple(map(evaluate, exps, repeat(env)))

def call_func(func, args, env) :
	'func: a Func object; args: a List'
	if type(func) == Symbol :
		func = find_func(func.value, env)
	return func(args, env)

def find_func(name, env) :
	'Find function from builtin and environments'
	if name in functions :
		return functions[name]
	matched = re.fullmatch('C([AD]{1,4})R', name)
	if matched :
		return caordr(matched.groups()[0])
	for i in reversed(env) :
		if i.has_fun(name) :
			return i.get_fun(name)
	raise NameError('Cannot find function: %s' % name)

def find_var(name, env) :
	'Find variable from environments'
	for i in reversed(env) :
		if i.has_var(name) :
			return i.get_var(name)
	raise NameError('Cannot find variable: %s' % name)

def to_bool(value) :
	if value == True :
		return Bool()
	elif value == False :
		return List()
	else :
		raise ValueError('Expect Python bool value, but get %s' % repr(value))

def is_true(value) :
	return not(type(value) == List and value.nil())

def build_list(l) :
	# TODO: remove the use of reversed
	ans = List()
	for i in reversed(l) :
		ans = List(i, ans)
	return ans

quoter = lambda x: List(Symbol('quote'), List(x, List()))

# Arithmetics

def plus(exps, env) :
	'(+ 1 2 3)'
	nums = eval_params(exps, env)
	return functools.reduce(operator.add, nums)

def minus(exps, env) :
	'(- 1 2)'
	nums = eval_params(exps, env)
	if len(nums) == 1 :
		a, = nums
		assert type(a) == Number
		return operator.neg(a)
	elif len(nums) == 2 :
		a, b = nums
		assert type(a) == Number and type(b) == Number
		return operator.sub(a, b)
	else :
		raise Exception('Invalid variable numbers for - function')

def mul(exps, env) :
	'(* 1 2 3)'
	nums = eval_params(exps, env)
	return functools.reduce(operator.mul, nums)

def div(exps, env) :
	'(/ 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return operator.truediv(a, b)

def one_plus(exps, env) :
	'(1+ 2)'
	a, = eval_params(exps, env)
	assert type(a) == Number
	return a + Number(1)

def eq_(exps, env) :
	'(= 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return to_bool(a.equal(b))

def lt(exps, env) :
	'(< 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return to_bool(operator.lt(a, b))

def le(exps, env) :
	'(<= 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return to_bool(operator.le(a, b))

def gt(exps, env) :
	'(> 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return to_bool(operator.gt(a, b))

def ge(exps, env) :
	'(>= 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return to_bool(operator.ge(a, b))

# Unary Predicates

def null(exps, env) :
	'(null NIL) -> T'
	value, = eval_params(exps, env)
	return to_bool(type(value) == List and value.nil())

def atom(exps, env) :
	'(atom NIL) -> T'
	value, = eval_params(exps, env)
	return to_bool(issubclass(type(value), Atom) or 
					type(value) == List and value.nil())

def listp(exps, env) :
	'(listp ()) -> T'
	value, = eval_params(exps, env)
	return to_bool(type(value) == List)

def zerop(exps, env) :
	'(zerop ()) -> T'
	value, = eval_params(exps, env)
	assert type(value) == Number
	return to_bool(value.value == 0)

# Binary Predicates

def eq(exps, env) :
	'(eq 1 2)'
	return eql(exps, env)

def eql(exps, env) :
	'(eql 1 2)'
	a, b = eval_params(exps, env)
	ta, tb = type(a), type(b)
	if ta != tb :
		return to_bool(False)
	if ta == Number :
		return to_bool(a.equal(b))
	elif ta == List :
		return to_bool(id(a) == id(b) or a.nil() and b.nil())
	elif ta == Dot :
		return to_bool(True)
	elif ta == Symbol :
		return to_bool(a.value == b.value)
	elif ta == Bool :
		return to_bool(True)
	else :
		raise ValueError('Unexpected type: %s' % repr(ta))

def equal(exps, env) :
	"(equal '(1) '(1))"
	a, b = eval_params(exps, env)
	ta, tb = type(a), type(b)
	if ta != tb :
		return to_bool(False)
	if type(a) == Number :
		return to_bool(a.equal(b))
	elif type(a) == List :
		if a.nil() :
			return to_bool(b.nil())
		if b.nil() :
			return to_bool(False)
		if not equal(List(quoter(a.car), List(quoter(b.car), List())), env) :
			return to_bool(False)
		return equal(List(quoter(a.cdr), List(quoter(b.cdr), List())), env)
	elif type(a) == Dot :
		return to_bool(True)
	elif type(a) == Symbol :
		return to_bool(a.value == b.value)
	elif type(a) == Bool :
		return to_bool(True)
	else :
		raise ValueError('Unexpected type: %s' % repr(ta))

# Logic

def and_(exps, env) :
	'(and 1 2 3)'
	for value in exps :
		if not is_true(evaluate(value, env)) :
			return to_bool(False)
	return to_bool(True)

def or_(exps, env) :
	'(or 1 2 3)'
	for value in exps :
		if is_true(evaluate(value, env)) :
			return to_bool(True)
	return to_bool(False)

def not_(exps, env) :
	'(not 1)'
	value, = eval_params(exps, env)
	return to_bool(not is_true(value))

# List operations

def car(exps, env) :
	"(car '(1 2 3)) -> 1"
	l, = eval_params(exps, env)
	assert type(l) == List
	return l.get_car()

def cdr(exps, env) :
	"(cdr '(1 2 3)) -> (2 3)"
	l, = eval_params(exps, env)
	assert type(l) == List
	return l.get_cdr()

def caordr(func_name) :
	"(cadddr '(1 2 3 4 5)) -> 4"
	def answer(exps, env) :
		l, = eval_params(exps, env)
		for i in order :
			assert type(l) == List
			if i == 'A' :
				l = l.get_car()
			else :
				l = l.get_cdr()
		return l

	order = list(reversed(func_name))
	return answer

def cons(exps, env) :
	"(cons '1 '(2 3)) -> (1 2 3); (cons 1 2) -> (1 . 2)"
	a, d = eval_params(exps, env)
	return List(a, d)

def list_(exps, env) :
	"(list '1 'a) -> (1 A)"
	params = eval_params(exps, env)
	return build_list(params)

# High-Order Functions

def mapcar(exps, env) :
	"(mapcar #'+ '(1 2 3) '(10 20 30))"
	args = eval_params(exps, env)
	func = args[0]
	params = args[1:]
	assert all(map(lambda x: type(x) == List, params))
	answer = []
	while not any(map(lambda x: x.nil(), params)) :
		arg = build_list(list(map(lambda x: quoter(x.car), params)))
		answer.append(call_func(func, arg, env))
		params = list(map(lambda x: x.cdr, params))
	return build_list(answer)

def mapc(exps, env) :
	"(mapc #'+ '(1 2 3) '(10 20 30)) -> '(1 2 3)"
	args = eval_params(exps, env)
	func = args[0]
	params = args[1:]
	assert all(map(lambda x: type(x) == List, params))
	while not any(map(lambda x: x.nil(), params)) :
		arg = build_list(list(map(lambda x: quoter(x.car), params)))
		call_func(func, arg, env)
		params = list(map(lambda x: x.cdr, params))
	return args[1]

def maplist(exps, env) :
	"(maplist #'cons '(2 3) '(20 30)) -> (((2 3) 20 30) ((3) 30))"
	args = eval_params(exps, env)
	func = args[0]
	params = args[1:]
	assert all(map(lambda x: type(x) == List, params))
	answer = []
	while not any(map(lambda x: x.nil(), params)) :
		arg = build_list(list(map(quoter, params)))
		answer.append(call_func(func, arg, env))
		params = list(map(lambda x: x.cdr, params))
	return build_list(answer)

def append(exps, env) :
	"(append '(1 2 3) '(4 5 6) '(7 8 9))"
	answer = []
	for i in eval_params(exps, env) :
		assert type(i) == List
		answer += list(i)
	return build_list(answer)

# Functions

def defun(exps, env) :
	'(defun f (x) (* x x))'

	def result(exps_, env_) :
		new_env = Env()
		new_envs = env_ + [new_env]
		actual_args = eval_params(exps_, env_)
		for f, a in zip(formal_args, actual_args) :
			new_env.set_var(f.value, a)
		for stmt in f_stmt :
			ans = evaluate(stmt, new_envs)
		return ans

	f_name = exps.car
	assert type(f_name) == Symbol
	f_args = exps.cdr.car
	f_stmt = exps.cdr.cdr
	formal_args = list(f_args)
	assert all(map(lambda x: type(x) == Symbol, formal_args))
	env[0].set_fun(f_name.value, result)
	return f_name

def apply(exps, env) :
	"(apply #'+ '(1 2 3))"
	apply_args = eval_params(exps, env)
	func = apply_args[0]
	args = apply_args[1:-1] + tuple(apply_args[-1])
	arg_list = build_list(list(map(quoter, args)))
	return call_func(func, arg_list, env)

def funcall(exps, env) :
	"(funcall #'+ 1 2 3)"
	func = evaluate(exps.car, env)
	args = exps.cdr
	return call_func(func, args, env)

def function(exps, env) :
	"(function +) OR #'+"
	name, = exps
	assert type(name) == Symbol
	return find_func(name.value, env)

def quote(exps, env) :
	"(quote (1 2 3)) OR '(1 2 3)"
	assert exps.cdr.nil()
	return exps.car

def eval_(exps, env) :
	'(eval (+ 1 2 3))'
	return evaluate(exps.car, env)

# Variables

def let(exps, env) :
	'(let ((x 10)) (* x 2))'
	new_env = Env()
	new_envs = env + [new_env]
	assert type(exps.car) == List
	for i in exps.car :
		assert type(i) == List
		assert i.cdr.cdr.nil()
		new_env.set_var(i.car.value, evaluate(i.cdr.car, env))
	ans = List()
	for i in exps.cdr :
		ans = evaluate(i, new_envs)
	return ans

def let_star(exps, env) :
	'(let* ((x 10) (y (1+ x))) (* y 2)) -> 22'
	new_env = Env()
	new_envs = env + [new_env]
	assert type(exps.car) == List
	for i in exps.car :
		assert type(i) == List
		assert i.cdr.cdr.nil()
		new_env.set_var(i.car.value, evaluate(i.cdr.car, new_envs))
	ans = List()
	for i in exps.cdr :
		ans = evaluate(i, new_envs)
	return ans

def setq(exps, env) :
	'(setq x 10) -> X'
	assert type(exps) == List
	assert exps.cdr.cdr.nil()
	k = exps.car.value
	v = evaluate(exps.cdr.car, env)
	for e in reversed(env) :
		if e.has_var(k) :
			break
	e.set_var(exps.car.value, evaluate(exps.cdr.car, env))
	return k

# Conditions

def cond(exp, env) :
	'(cond ((> 1 2) 1) ((< 1 2) 2))'
	for test in exp :
		assert type(test) == List
		if is_true(evaluate(test.car, env)) :
			for stmt in test.cdr :
				ans = evaluate(stmt, env)
			return ans
	return List()

# Evaluate

def evaluate(exp, env) :
	'exp is (+ 2 3) OR 2 OR jkl'
	te = type(exp)
	if te == List :
		if exp.nil() :
			return exp
		assert type(exp.car) == Symbol
		func = find_func(exp.car.value, env)
		return call_func(func, exp.cdr, env)
	elif te == Number :
		return exp
	elif te == Symbol :
		return find_var(exp.value, env)
	elif te == Bool :
		return exp
	else :
		assert isinstance(exp, Atom)
		raise ValueError('Unexpected type: %s' % repr(te))

# Index

functions = {
	'+': plus, 				# Arithmetics
	'-': minus, 
	'*': mul, 
	'/': div, 
	'1+': one_plus, 
	'=': eq_, 
	'<': lt, 
	'<=': le, 
	'>': gt, 
	'>=': ge, 
	'NULL': null, 			# Unary Predicates
	'ATOM': atom, 
	'LISTP': listp, 
	'ZEROP': zerop, 
	'EQ': eq, 				# Binary Predicates
	'EQL': eql, 
	'EQUAL': equal, 
	'AND': and_, 			# Logic
	'OR': or_, 
	'NOT': not_, 
	'CAR': car, 			# List operations
	'CDR': cdr, 
	'CONS': cons, 
	'LIST': list_, 
	'MAPCAR': mapcar, 		# High-Order Functions
	'MAPC': mapc, 
	'MAPLIST': maplist, 
	'APPEND': append, 
	'DEFUN': defun, 		# Functions
	'APPLY': apply, 
	'FUNCALL': funcall, 
	'FUNCTION': function, 
	'QUOTE': quote, 
	'EVAL': eval_, 
	'LET': let, 			# Variables
	'LET*': let_star, 
	'SETQ': setq, 
	'COND': cond, 			# Conditions
}

