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
	Test file format: ...
	-> CLEAR-ENV
	=> CLEAR-ENV
'''

import sys
from structs import Env
from frontend import build_tree
from builtin import evaluate
from backend import eval_str

def test(file) :
	tests = []
	state = 0
	for ln in open(file).read().split('\n') :
		if not ln :
			continue
		elif ln.startswith('->') :
			tests.append([ln[2:].lstrip()])
		elif ln.startswith('=>') :
			tests[-1].append(ln[2:].lstrip())
		else :
			tests[-1][-1] += '\n' + ln
	env = [Env()]
	for q, a in tests :
		if q.strip().upper() == 'CLEAR-ENV' :
			env = [Env()]
			print('-> CLEAR-ENV\n=> CLEAR-ENV\n')
			continue
		for qq, aa in zip(build_tree(q), build_tree(a)) :
			aaa = str(aa)
			if aaa.strip() == 'ERROR' :
				error_flag = False
				try :
					print('->', qq)
					print('=>', evaluate(qq, env))
				except Exception :
					error_flag = True
					print('=> ERROR')
				if not error_flag :
					raise Exception('Test fails: should error')
			else :
				print('->', qq)
				qqq = str(evaluate(qq, env))
				print('=>', qqq)
				if qqq != aaa :
					print('!>', aa)
					raise Exception('Test fails: wrong answer')
		print()

if __name__ == '__main__' :
	for i in sys.argv[1:] :
		test(i)

