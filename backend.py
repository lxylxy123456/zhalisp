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

