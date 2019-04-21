def f(n=10) :
	print('(and')
	for i in range(-n, n) :
		for j in range(-n, n) :
			print(' (= (+ (print %d) (print %d)) (plus %d %d))' % (i, j, i, j))
			print(' (= (- (print %d) (print %d)) (minus %d %d))' % (i, j, i, j))
	print(')')

f(10)

