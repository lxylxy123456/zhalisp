from structs import Env, Atom, Number, Symbol, List
from frontend import build_tree
import builtin

if __name__ == '__main__' :
	if 1 :
		for s, a in [
			('(quote (123.67 (1 2 . (3 4)) (1 2 3 . 4)))', 
				'(123.67 (1 2 . (3 4)) (1 2 3 . 4))'), 
			('(+ 1 2 3 (/ 3 4) (* 1 3) (- 90 3))\n123', '387/4\n123'), 
			('(defun f (x) (* 9 x) (+ 2 x))\n(f 2)\n(f 4)', 'F\n4\n6'), 
			("(funcall #'+ 1 2 3)", '6'), 
			("(apply #'+ 1 2 3 '(4 5 6))", '21'), 
			("(apply 'cons '(a (b c)))", "(A B C)"), 
			("(= 1 2)", "NIL"), 
			("(= 2 2)", "T"), 
			("(> 1 2)", "NIL"), 
			("(>= 1 2)", "NIL"), 
			("(list (< 1 2) (<= 1 2))", "(T T)"), 
			('(cond ((> 1 2) 1) ((< 1 2) 2))', '2'), 
			('(let ((x 10)) (* x 2))', '20'), 
			('(setq x 10)\nx', 'X\n10'), 
			('(eq 1 2)', 'NIL'), 
			('(eql 1 2)', 'NIL'), 
			('(equal 1 2)', 'NIL'), 
			('(eq 1 1)', 'NIL'), 
			('(eql 1 1)', 'T'), 
			('(equal 1 1)', 'T'), 
			("(eq '(1) '(1))", 'NIL'), 
			("(eql '(1) '(1))", 'NIL'), 
			("(equal '(1) '(1))", 'T'), 
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
	if 1 :
		from parent_dir import f_list, get_path, tests
		env = [Env()]
		for f in f_list :
			p = lambda *x: x
			# p = print
			p(';', f)
			s = open(get_path(f)).read()
			a = build_tree(s)
			for i in a :
				p(builtin.evaluate(i, env))
			p()
		for s, a in tests :
			ss = build_tree(s)
			aa = build_tree(a)
			for sss, aaa in zip(ss, aa) :
				e1 = builtin.evaluate(sss, env)
				e2 = aaa
				print(e1, e2, sep='\t')
				if str(e1) != str(e2) :
					raise Exception

