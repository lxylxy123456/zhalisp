'''
	Draw dependency graph of header files
'''

import os, re

def grep_include(filename) :
	for i in open(filename).read().split('\n') :
		matched = re.fullmatch('\s*#include "([\w\.\/]+)"', i)
		if matched :
			yield matched.groups()[0]

def gen_edges(root='.') :
	for p, d, f in os.walk(root) :
		for i in f :
			if os.path.splitext(i)[1] == '.h' :
				for j in grep_include(os.path.join(p, i)) :
					yield (
						os.path.splitext(i)[0], 
						os.path.splitext(os.path.split(j)[1])[0], 
					)

if __name__ == '__main__' :
	print('digraph G {')
	for i in gen_edges() :
		print('\t%s -> %s' % i)
	print('}')

