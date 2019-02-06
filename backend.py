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

from structs import Env
from frontend import build_tree
from builtin import evaluate
import traceback

def eval_str(s, env) :
	'build sexp from str and evaluate'
	for i in build_tree(s) :
		yield evaluate(i, env)

def shell(env=None) :
	'''
		Interactive mode, returns the final environment upon exit
		Use 'CLEAR-ENV' to clear environment
		Use '(EXIT)' to exit
	'''
	if env == None :
		env = [Env()]
	while True :
		print('-> ', end='', flush=True)
		try :
			s = input()
		except EOFError :
			print()
			break
		while True :	# LIMIT: when the command line is multiple lines,
			try :		#        will parse it after each line of input is made
				t = build_tree(s)
				break
			except SyntaxError :
				s += '\n' + input()
		if s.strip().upper() == '(EXIT)' :
			break
		elif s.strip().upper() == 'CLEAR-ENV' :
			env = [Env()]
			print('=> CLEAR-ENV')
			continue
		for i in t :
			try :
				ans = evaluate(i, env)
				print('=>', ans)
			except Exception :
				traceback.print_exc()
	return env

if __name__ == '__main__' :
	# for i in eval_str('(defun f (x) (+ 2 x))\n(f 2)\n(f 4)',[Env()]): print(i)
	shell()

