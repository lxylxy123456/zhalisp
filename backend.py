from structs import Env, Atom, Number, Symbol, List
from frontend import build_tree
import builtin

def show_sexp(sexp, env) :
	l = sexp.to_list()
	return l

if __name__ == '__main__' :
	if 1 :
		for s, a in [
			# ('(123.67 (1 2 . (3 4)) (1 2 3 . 4))', ), 
			('(+ 1 2 3 (/ 3 4) (* 1 3) (- 90 3))\n123', '387/4\n123'), 
			('(defun f (x) (* 9 x) (+ 2 x))\n(f 2)', 'f\n4'), 
			] :
			env = [Env()]
			ss = build_tree(s)
			aa = build_tree(a)
			for sss, aaa in zip(ss, aa) :
				e1 = builtin.evaluate(sss, env)
				e2 = aaa
				print(e1, e2, sep='\t')
				if str(e1) != str(e2) :
					raise Exception
	if 0 :
		from parent_dir import f_list, get_path
		for f in f_list :
			print(';', f)
			s = open(get_path(f)).read()
			a = build_tree(s)
			env = [Env()]
			for i in a :
				print(builtin.evaluate(i, env))
			print()

