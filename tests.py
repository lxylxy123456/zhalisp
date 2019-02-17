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

'''
	Test file format
		-> test expression to be evaluated
		=> answer expression (not to be evaluated)
	Example
		-> (+ 1 2)
		=> 3
	Printing
		-> (+ (print 1) 2)
		=> 1
			3
	To clear environment
		-> CLEAR-ENV
		=> ...
'''

import sys
from itertools import zip_longest

from structs import Env
from frontend import build_tree
from builtin import evaluate
from backend import eval_str

def build_test_env(stdout) :
	env = [Env()]
	env[0].print = lambda x: stdout.append(x)
	return env

def match(a, b) :
	return str(a) == str(b)

def test(file) :
	tests = []
	for ln in open(file).read().split('\n') :
		if not ln :
			continue
		elif ln.startswith('->') :
			tests.append([ln[2:].lstrip()])
		elif ln.startswith('=>') :
			tests[-1].append(ln[2:].lstrip())
		else :
			tests[-1][-1] += '\n' + ln
	stdout = []
	env = build_test_env(stdout)
	for q, a in tests :
		if q.strip().upper() == 'CLEAR-ENV' :
			env = build_test_env(stdout)
			print('-> CLEAR-ENV\n=> CLEAR-ENV\n')
			continue
		aa = iter(build_tree(a))
		for qq in build_tree(q) :
			aaa = str(next(aa, None))
			# when None, will raise error, so user can see qq evaluated
			if aaa.strip() == 'ERROR' or aaa.strip() == 'ERRORC' :
				error_flag = False
				try :
					print('->', qq)
					print('=>', evaluate(qq, env))
				except Exception :
					error_flag = True
					print('=>', aaa.strip())
				if not error_flag :
					raise Exception('Test fails: should error')
			else :
				stdout.clear()
				print('->', qq)
				qqq = str(evaluate(qq, env))
				for i in stdout :
					print('p>', i)
					if not match(i, aaa) :
						raise Exception('Test fails: wrong print output')
					aaa = str(next(aa, None))
				print('=>', qqq)
				if qqq != aaa :
					print('!>', aaa)
					raise Exception('Test fails: wrong answer')
		assert next(aa, None) == None
		print()

if __name__ == '__main__' :
	for i in sys.argv[1:] :
		test(i)

