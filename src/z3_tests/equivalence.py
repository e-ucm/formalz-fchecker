# Prove equivalence of two formulas containing 'forall'

from z3 import *

# Checks if "x > 0" is equivalent to "x - 1 >= 0" (should print "no solution")
def eq():
	x = Int('x')
	ast1 = x > 0
	print "ast1: ", ast1

	ast2 = x - 1 >= 0
	print "ast2: ", ast2

	ast = ast1 != ast2
	print ast

	print "model:"
	solve(ast)

# Checks if "x > 0" is equivalent to "x + 1 >= 0" (should give a counter example)
def neq():
	x = Int('x')
	ast1 = x > 0
	print "ast1: ", ast1

	ast2 = x + 1 >= 0
	print "ast2: ", ast2

	ast = ast1 != ast2
	print ast

	print "model:"
	solve(ast)

# Checks if "ForAll(x, x > 0) == ForAll(x, x < 0)", should give a counter example but doesn't!
def neq_forall():
	x = Int('x')
	ast1 = ForAll(x, x > 0)
	print "ast1: ", ast1

	ast2 = ForAll(x, x < 0)
	print "ast2: ", ast2

	ast = ast1 != ast2
	print ast

	print "model:"
	solve(ast)

eq()
print ""
neq()
print ""
neq_forall()