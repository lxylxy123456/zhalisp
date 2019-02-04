'''
	builtin functions in lisp
	LIMIT: complex + fraction will have different behavior than Clisp
'''

import functools, operator

from structs import Env, Atom, Number, Symbol, List
from itertools import repeat

# TODO: to_list -> generator
# TODO: add function for `map(evaluate, exps.to_list(), repeat(env))`

def plus(exps, env) :
	# (+ 1 2 3)
	nums = map(evaluate, exps.to_list(), repeat(env))
	return functools.reduce(operator.add, nums)

def minus(exps, env) :
	# (- 1 2)
	a, b = map(evaluate, exps.to_list(), repeat(env))
	return operator.sub(a, b)

def mult(exps, env) :
	# (* 1 2 3)
	nums = map(evaluate, exps.to_list(), repeat(env))
	return functools.reduce(operator.mul, nums)

def div(exps, env) :
	# (/ 1 2)
	a, b = map(evaluate, exps.to_list(), repeat(env))
	return operator.truediv(a, b)

def defun(exps, env) :
	# (defun f (x) (* x x))

	def result(exps, env) :
		new_env = Env()
		new_envs = env + [new_env]
		actual_args = map(evaluate, exps.to_list(), repeat(env))
		for f, a in zip(formal_args, actual_args) :
			new_env.set_var(f.value, a)
		for stmt in f_stmt.to_list() :
			ans = evaluate(stmt, new_envs)
		return ans

	f_name = exps.car
	assert type(f_name) == Symbol
	f_args = exps.cdr.car
	f_stmt = exps.cdr.cdr
	formal_args = f_args.to_list()
	assert all(map(lambda x: type(x) == Symbol, formal_args))
	env[0].set_fun(f_name.value, result)
	return f_name

def call_func(func, args, env) :
	# func: a Func object; args: a List
	return func(args, env)

def apply(exps, env) :
	# (apply #'+ '(1 2 3))
	func = evaluate(exps.car)
	args = 0/0

def funcall(exps, env) :
	# (funcall #'+ 1 2 3)
	func = evaluate(exps.car)
	args = sexp.cdr
	return call_func

def eval_(exps, env) :
	# (eval (+ 1 2 3))
	return evaluate(exps.car, env)

def evaluate(exp, env) :
	# exp is (+ 2 3) OR 2 OR jkl
	if type(exp) == List :
		if type(exp.car) == Symbol :
			fun_name = exp.car.value
			if fun_name in functions :
				func = functions[fun_name]
			else :
				func = None
				for i in reversed(env) :
					if i.has_fun(fun_name) :
						func = i.get_fun(fun_name)
						break
				assert func != None
		else :
			func = 0/0
		return call_func(func, exp.cdr, env)
	elif type(exp) == Number :
		return exp
	elif type(exp) == Symbol :
		var_name = exp.value
		for i in reversed(env) :
			if i.has_var(var_name) :
				return i.get_var(var_name)
		raise NameError
	else :
		assert isinstance(exp, Atom)

functions = {
	'+': plus, 
	'-': minus, 
	'*': mult, 
	'/': div, 
	'defun': defun, 
	'funcall': funcall, 
	'eval': evaluate, 
}

