tests = [
	('(quote (123.67 (1 2 . (3 4)) (1 2 3 . 4)))', 
		'(123.67 (1 2 . (3 4)) (1 2 3 . 4))'), 
	('(+ 1 2 3 (/ 3 4) (* 1 3) (- 90 3))\n123', '387/4\n123'), 
	('(defun f (x) (* 9 x) (+ 2 x))\n(f 2)\n(f 4)', 'F\n4\n6'), 
	("(funcall #'+ 1 2 3)", '6'), 
	("(apply #'+ 1 2 3 '(4 5 6))", '21'), 
	("(apply 'cons '(a (b c)))", '(A B C)'), 
	('(= 1 2)', 'NIL'), 
	('(= 2 2)', 'T'), 
	('(> 1 2)', 'NIL'), 
	('(>= 1 2)', 'NIL'), 
	('(list (< 1 2) (<= 1 2))', '(T T)'), 
	('(cond ((> 1 2) 1) ((< 1 2) 2))', '2'), 
	('(let ((x 10)) (* x 2))', '20'), 
	('(setq x 10)\nx', 'X\n10'), 
	('(eq 1 2)', 'NIL'), 
	('(eql 1 2)', 'NIL'), 
	('(equal 1 2)', 'NIL'), 
	('(eq 1 1)', 'T'), 
	('(eql 1 1)', 'T'), 
	('(equal 1 1)', 'T'), 
	("(eq '(1) '(1))", 'NIL'), 
	("(eql '(1) '(1))", 'NIL'), 
	("(equal '(1) '(1))", 'T'), 
	("(cadddr '(1 2 3 4 5))", '4'), 
	('(or nil nil 1 (/ 0 0))', 'T'), 
	('(and 1 1 nil (/ 0 0))', 'NIL'), 
	('(or 1 1 1)', 'T'), 
	('(and 1 1 1)', 'T'), 
	('(or nil nil nil)', 'NIL'), 
	('(and nil nil nil)', 'NIL'), 
	('(not nil)', 'T'), 
	('(not 1)', 'NIL'), 
	('t', 't'), 
	# Credit of test cases below goes to Professor Aditya V. Thakur
	("(append '(1 2 3) '(4 5 6) '(7 8 9))", '(1 2 3 4 5 6 7 8 9)'), 
	("(mapcar #'+ '(1 2 3) '(10 20 30))", '(11 22 33)'), 
	("(mapcar #'+ nil)", '()'), 
	("(mapc #'+ '(1 2 3) '(10 20 30))", '(1 2 3)'), 
	("(car (cdr '(a b c)))", 'b'), 
	("(atom (cdr (cdr '(a))))", 'T'), 
	("(mapcar #'- '(1 2 3))", '(-1 -2 -3)'), 
	("(mapcar 'list '(10 2 5))", '((10) (2) (5))'), 
	("""(defun bar (x) (list x (1+ x)))
		(defun mappend (fn lst) (apply #'append (mapcar fn lst)))
		(mappend #'bar '(10 1 3))""", 
	 '''bar
		mappend
		(10 11 1 2 3 4)'''), 
	("(cons '(1 2) '3)", '((1 2) . 3)'), 
	("(maplist #'cons '(a b) '(x y))", '(((A B) X Y) ((B) Y))'), 
	('(let ((a 2)) (let ((a 3) (b (+ a 1))) (+ a b)))', '6'), 
	('(cond ((> 1 3) 1) (( > 1 2) 2))', 'NIL'), 
	('(listp (zerop 1))', 'T'), 
	# Credit of test cases above goes to Professor Aditya V. Thakur
]

from structs import Env
from frontend import build_tree
from builtin import evaluate

def test() :
	for s, a in tests :
		env = [Env()]
		ss = build_tree(s)
		aa = build_tree(a)
		for sss, aaa in zip(ss, aa) :
			e1 = evaluate(sss, env)
			e2 = aaa
			print(e1, e2, sep='\t')
			if str(e1) != str(e2) :
				print(s)
				raise Exception

