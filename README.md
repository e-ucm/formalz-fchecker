# IMPRESS EDSL

**This document still needs reviewing!**

We want to help students learn about formal program verification. One aspect of this is writing pre and post conditions for their programs. To help the students learn this we developed a tool that can compare two program specifications and can come up with a counter example if the two specifications don't match.

One use case could be to integrate our tool in a submission system like [DOMjudge](https://www.domjudge.org). We then allow students to submit their programs and they get instant feedback on their work.

## Java EDSL

To get started we designed a simple embedded DSL that encapsulates all the expressions taught in the Software Testing & Verification (INFOB3STV) course. This includes:

- Integer expressions (addition, subtraction, multiplication, etc.)
- Boolean expressions (AND, OR, NOT)
- Relational operators (bigger, smaller, equal, etc.)
- Quantifications over integer domains
- Array access

### Example 1

```java
public static void swap_spec1(int[] a, int i, int j) {
    // preconditions
    pre(a != null);
    pre(a.length > 0);
    pre(i >= 0);
    pre(j >= 0);

    // introducing variables to remember old values
    int oldai = a[i], oldaj = a[j];

    // call the actual function implementation
    swap(a, i, j);

    // postconditions
    post(a[j] == oldai);
    post(a[i] == oldaj);
}
```

This example uses simple arithmetic and shows that multiple pre/post conditions are allowed. Internally they will be appened in a conjunction, but this allows the student to work more easily.

### Example 2

```java
public static void getMax_spec1(int[] a) {
    // preconditions
    pre(a != null);
    pre(a.length > 0);
    
    // call the actual function implementation
    int retval = getMax(a);
    
    // postconditions
    post(exists(a, i -> a[i] == retval)); // A
    post(forall(a, i -> a[i] <= retval)); // B
}
```

This example uses the EDSL quantifier functions `exists` and `forall`. In addition to a meaning for the DSL, they also have runtime implementations that allows you to test conditions with concrete values.

Postcondition A will be mapped to:

![Postcondition A](https://i.imgur.com/YBZ178V.png)

Postcondition B will be mapped to:

![Postcondition B](https://i.imgur.com/JZDVSvy.png)

### Notes

- Logical implication is currently not supported, but should be easy to add in the DSL is required.

## LogicIR

![LogicIR diagram](https://i.imgur.com/fBUDkGY.png)

To facilitate further development, it was decided to use an intermediate representation for logical expressions. The frontend converts the Java EDSL to the IR and the backend uses IR to implement the comparison of two expressions.

You can find the source code in `src/LogicIR/Expr.hs` TODO N is numeric

TODO: future work, sets, types
TODO: parse logicir from file

### LogicIR.Frontend

Currently there is only one frontend for the Java EDSL, but this could quite easily be extended to other programming languages.

### LogicIR.Backend.Z3

One of the implemented backends is for the [Z3 Theorem Prover](https://github.com/Z3Prover/z3). The `LogicIR.Expr` is converted to a `Z3 AST`.

To determine if the expression `P == Q` is valid, we ask Z3 to prove that `P != Q` is unsatisfiable. There are three possible results:

1. `Sat` -> Z3 proved `P != Q` is satisfiable, which means that the formula `P == Q` is invalid. The Z3 model contains the counter example to provide to the student.
2. `Unsat` -> Z3 proved that `P != Q` is not satisfiable, which means that the formula `P == Q` is valid.
3. `Undef` -> Z3 was unable to decide the satisfiablity of `P != Q`. In this case we have to resort to other methods like QuickCheck to determine if the two formulas are equivalent or not.

#### Notes

One of the assumptions we make is that both specifications are defined in functions that have the same variable and argument names. That way if we have an array `a` that is used in `P`, we know that if `Q` uses `a` that they refer to the same `a`. See [example 1](#example-1) where both specifications have to refer to the `retval`, `oldi` and `oldj` variables in order to allow Z3 to prove anything.

Something to be wary of when reasoning about this is that we are not trying to prove that an individual specification is satisfiable. We are merely interested in proving that two specifications are equal or not. That said, if you ask Z3 if `ForAll(i, i > 0) != ForAll(i, i < 0)` is satisfiable it will give back `Unsat`, because the formula can be reduced to `False != False` which is false. In practice this should be an issue, because the specification that the student's is compared to will be correct.

#### Null arrays

Currently the expression `a != null` is not really supported because Z3 does not have the concept of null arrays. As a workaround, `null` is represented as an infinite array that maps every index to a constant value that is very unlikely to occur in real code. (TODO: write a counter example). A proposed idea is to represent arrays as a tuple `(IsNull, ArrayData)`, but this requires more semantic analysis of the actual expression to be used. (TODO: give an example, note: usually expressions involving null are short-circuit, unsure...).

#### Array length

Similar to null arrays, Z3 does not (appear to) have the concept of an array length. Currently we use a free variable `a.length` that should represent the array length, but constraints should be added on every `a[E]` in the form of `E >= 0 && E < a.length` to make sure Z3 outputs the `a.length` that corresponds to the array it modeled.

#### Bit vectors

> Modern CPUs and main-stream programming languages use arithmetic over fixed-size bit-vectors. The theory of bit-vectors allows modeling the precise semantics of unsigned and of signed two-complements arithmetic. There are a large number of supported functions and relations over bit-vectors.
> 
> https://rise4fun.com/z3/tutorialcontent/guide#h25

To implement the semantics of Java (and C#, C++, etc) we use Z3 bit vectors to represent integers. Things like overflow semantics are then handled correctly by Z3.

#### Type checking and type limitations

Currently the implementation only supports the following types:

`int`, `bool`, `int[]`, `bool[]`

Adding additional types (like various integer sizes, `float`) would require some kind of type analysis, which is possible, but would take a considerable amount of time to implement

#### Z3 model parsing

When comparing:

```
e1:
exists(a, i -> a[i] == retval) && forall(a, i -> a[i] <= retval)

e2:
exists(a, i -> a[i] == retval) && forall(a, i -> a[i] < retval)
```

Z3 will report that `e1 != e2` with the model:

```
i!1 -> #x00000000
a.length -> #x00000001
a -> (_ as-array k!3)
i!2 -> #x00000000
retval -> #x00000000
k!3 -> {
  #x00000000
}
```

This is rather unusable from a user perspective, it should report something like:

```
Counter example:

a: [0]
retval: 0
```

This is currently a work in progress. TODO

#### Side effects

Expressions like `i++ > 0 && i > 1` are not supported because they are considered out of scope for this project.

#### Relevant resources

- https://git.science.uu.nl/d.h.ogilvie2/javawlp/wikis/Resources-about-z3
- https://git.science.uu.nl/d.h.ogilvie2/javawlp/wikis/home

## TODO

TODO: document usage of haskell code
TODO: document haskell code itself better
TODO: latex (https://www.overleaf.com/11599874kpwfhwctdqkr#/43883318/)
