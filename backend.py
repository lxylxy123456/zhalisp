from structs import Env, Atom, Number, Symbol, List
from frontend import build_tree
import builtin

if __name__ == '__main__' :
	if 1 :
		from tests import tests
		for s, a in tests :
			env = [Env()]
			ss = build_tree(s)
			aa = build_tree(a)
			for sss, aaa in zip(ss, aa) :
				e1 = builtin.evaluate(sss, env)
				e2 = aaa
				print(e1, e2, sep='\t')
				if str(e1) != str(e2) :
					print(s)
					raise Exception

