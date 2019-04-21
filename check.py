import os

print(open('bit_operation.lisp').read())

print('''
(defun checkab (a b)
 (and
  (print (list a b))
  (print '+)
  (= (+ a b) (plus a b))
  (print '-)
  (= (- a b) (minus a b))
  (print '*)
  (= (* a b) (multiply a b))
 )
)
''')

def f(n=10) :
	print('(and')
	for i in range(-n, n) :
		for j in range(-n, n) :
			print(' (checkab %d %d)' % (i, j))
	print(')')

f(40)

