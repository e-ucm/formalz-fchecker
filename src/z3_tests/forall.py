# Based on "Quantifiers" at https://ece.uwaterloo.ca/~agurfink/ece653/z3py-advanced

from z3 import *

f = Function('f', IntSort(), IntSort(), IntSort())
print "f: ", Z3_func_decl_to_string(f.ctx.ref(), f.ast)

x = Int('x')
ast1 = ForAll(x, f(x, x) == 0)
print "ast1: ", ast1

a = Int('a')
b = Int('b')
ast2 = f(a, b) == 1
print "ast2: ", ast2

print "\nmodel:"
solve(ast1, ast2)