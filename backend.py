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

def eval_str(s, env) :
	'build sexp from str and evaluate'
	for i in build_tree(s) :
		yield evaluate(i, env)

if __name__ == '__main__' :
	s = '(defun f (x) (+ 2 x))\n(f 2)\n(f 4)'
	for i in eval_str(s, [Env()]) :
		print(i)

