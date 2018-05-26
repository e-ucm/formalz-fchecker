## EDSL specification

The Java EDSL allows us to formulate logical properties involving variables in
the scope of the program body.

### Pre/post-conditions

These logical statements can be placed in the beginning(end)
of the program by using the `pre`(`post`) functions, e.g.
```java
public static void fun(int x) {
  pre(x == 1);
  x++;
  post(x == 2);
}
```

**NOTES**
- The inner expression in a pre/post-condition must be of type `Boolean`.
- If the argument to `pre`(`post`) is left empty or there is no definition, the logical expression is
defaulted to `true`.
- Multiple declaration of a pre/post-conditions will be combined with Logical
conjunction.

### Supported Java types

We currently support the following built-in Java types:
 * Booleans, i.e. `boolean`
 * Integers, i.e. `int`, `short`, `long`
 * Reals, i.e. `float`, `double`
 * Multi-dimensional arrays of the above types, e.g. `int[]`, `double[][]`

### Arithmetic expressions

We support the standard operations on arithmetic types (`Integer`, `Real`):
 * `|+|-|*|/|%|`: binary operator, e.g. `1 + (1 - 10 * 3/1)`
 * `|-|`: unary operator, e.g. `-(1 * 2)`
 * `|>|<|>=|<=||`: arithmetic comparison (the resulting type is `Boolean`)

### Polymorphic equality

Any supported type can be compared for equality using the `==`/`!=` operator, e.g.
```java
public static void fun(int x, boolean b) {
  pre(x == 1);
  pre(b == true);
  if (x == 1) {
    b = !b;
    x--;
  }
  post(b == false);
  post(x != 1);
}
```

There is also a special form for performing null-checking on array **variables**, e.g. `arr != null`

### Boolean expressions

Every pre/post-condition must be of `Boolean` type.

The operations allowed on Booleans are:
 * `&&`: logical conjunction, e.g. `true && false`
 * `||`: logical disjunction, e.g. `true || false`
 * `!`: logical negation, e.g. `!(true || false)`
 * `imp`: logical implication, e.g. `imp(false, true && false)`

Here is an example specification making use of these operators:
```java
public static void fun(boolean b) {
  pre(imp(true, true && b));
  b = !b || false;
  post(b == false);
}
 ```

### Array operations

There is a fairly minimal syntax for array operations:
 * `a.length`: retrieving the length of an array **variable**, e.g.

 ```java
 public static void fun(double[] a, boolean b)
 pre(imp(b, a.length > 0));
 post();
 ```

 * `a[...]`: indexing an array **variable** with an expression of type `Integer`, e.g.

 ```java
 public static void fun(double[] a)
 pre(a[0] == 0);
 a[0] = 2 * a[0] + 5
 post(a[1 - 1] == 5);
 ```

 * `forall`: asserting a property of all elements of an array **variable**, e.g.

 ```java
 public static void fun(int[] a)
 pre(forall(a, i -> a[i] == 0));
 for (int i = 0; i < a.length ; i++) { a[i]++; }
 post(forall(a, i -> a[i] == 1));
 ```

### Variable introduction

The EDSL also supports introduction of intermediate variables
(think of the `let` statement) via the `with` function, which introduces a
fresh variable holding the value of the passed expression of any type and the given
lambda can use it as it pleases and any number of times.

Here is an example usage that allows indexing multi-dimensional arrays:
```java
public static void fun(float[][] a)
pre(forall(a, i -> with(a[i], ai -> forall(ai, j -> ai[j] == 0)));
for (int i = 0; i < a.length ; i++) {
  for (int j = 0; j < a[i].length ; j++) {
    a[i][j]++;
  }
}
post(forall(a, i -> with(a[i], ai -> forall(ai, j -> ai[j] == 1)));
```
