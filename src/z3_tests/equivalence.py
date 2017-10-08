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
	s = Solver()

	x = Int(0)
	ast1 = ForAll(x, x > 0, patterns = [x])
	print "ast1: ", ast1

	y = Int(1)
	ast2 = ForAll(y, y < 0, patterns = [y])
	print "ast2: ", ast2

	s.add(ast1 == ast2)
	print s.check(), s.model()

# Check if "ForAll(x, x > 0) == ForAll(x, x < 0)" by evaluating "Exists(x, (x > 0) != (x < 0))"
def neq_forall2():
	x = Int('x')
	ast = (x > 0) != (x < 0)
	solve(Exists(x, ast, patterns = [x]))
	solve(ast)

def neq_forall3():
	return
	print "3"
	x = Int('x')
	ast1 = ForAll(x, x*x>=x, patterns = [x])
	ast2 = ForAll(x, x*x>=x-1, patterns = [x])
	s = Solver()
	s.add(Not(ast1) and ast2)
	print s.check(), s.model()

# A <=> B means A => B and B => A, so if we prove (A & ~B), A => B doesn't hold so A != B
# - joao

def python_is_super_hot():
	print "SUPER HOT"
	IntArray = ArraySort(IntSort(), IntSort())
	ArrayRef = Datatype('ArrayRef')
	ArrayRef.declare('NotNull', ('Value', IntArray))
	ArrayRef.declare('Null')
	ArrayRef = ArrayRef.create()

	print ArrayRef.Null
	print ArrayRef.NotNull
	nullArr = Array('null', IntSort(), IntSort())
	arr = Array('a', IntSort(), IntSort())
	solve (arr == nullArr)

help_simplify()

eq()
print ""
neq()
print ""
neq_forall()
print ""
neq_forall2()
print ""
neq_forall3()
print ""
python_is_super_hot()