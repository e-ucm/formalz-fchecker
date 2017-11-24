# IMPRESS EDSL

We want to help students learn about formal program verification. One aspect of this is writing pre and post conditions for their programs. To help the students learn this we developed a tool that can compare two program specifications and can come up with a counter example if the two specifications don't match.

One use case could be to integrate our tool in a submission system like [DOMjudge](https://www.domjudge.org). We then allow students to submit their programs and they get instant feedback on their work.

A proof of concept can be found in `src/SimpleFormulaChecker.hs`, the "main" function is `compareSpec`.

To compile the project you need to install following packages:

- [z3](https://hackage.haskell.org/package/z3) (see the [repository wiki](https://git.science.uu.nl/d.h.ogilvie2/javawlp/wikis/Installing-z3-haskell) for installation instructions)
- [alex](https://hackage.haskell.org/package/alex)
- [happy](https://hackage.haskell.org/package/happy)

Once you have the packages, run the following commands from the repository root to generate the lexer and parser for the models:

```
alex src/ModelParser/Lexer.x
happy src/ModelParser/Parser.y
```

You can now run GHCi from the `src` directory to run the examples.

## Java EDSL

To get started we designed a simple embedded DSL that encapsulates all the expressions taught in the Software Testing & Verification (INFOB3STV) course. This includes:

- Integer expressions (addition, subtraction, multiplication, etc.)
- Boolean expressions (AND, OR, NOT)
- Relational operators (bigger, smaller, equal, etc.)
- Quantifications over integer domains
- Array access

To import the EDSL in a Java project, we use:

```java
//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.EDSL.*;
```

An example Java project can be found in `examples/javawlp_edsl`, the EDSL implementation is in `impress_edsl/`.

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

- Logical implication is currently not supported, but should be easy to add in the EDSL is required.

## LogicIR

![LogicIR diagram](https://i.imgur.com/fBUDkGY.png)

To facilitate further development, it was decided to use an intermediate representation for logical expressions. The frontend converts the Java EDSL to the IR and the backend uses IR to implement the comparison of two expressions.

You can find the data types in `src/LogicIR/Expr.hs`. 

- TODO: future work, sets, types
- TODO: parse logicir from file
- TODO: implication, stronger, weaker

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

An expression like `a != null` cannot be supported by Z3 directly, because it does not have the concept of null arrays. As a workaround, `a == null` is represented as a free variable `a.null`, so an array essentially becomes a tuple `(a, a.null)`. To achieve this an additional preprocessing step `lExprPreprocessNull` is done after extracting the LogicIR expression.

Because `a` and `a.null` are not bound together by Z3 (accessing a null-array in Java will cause an exception), comparing two methods like this give curious results:

```
public static void null3(int[] a) {
    pre(a == null && a[0] > a[1]);
    post(true);
}

public static void test2(int[] a) {
	pre(false);
    post(true);
}
```

The resulting (raw) model:

```
a -> (_ as-array k!0)
a.null -> true
k!0 -> {
  #x00000000 -> #x00000001
  #x00000001 -> #x00000000
  else -> #x00000001
}
```

Essentially this model tells us that the two preconditions are not equivalent with an `a` that is both `null` and `[1, 0]`. A postprocessing step has been added to pretty print the model and it shows:

```
a = null
```

#### Array length

Similar to null arrays, Z3 does not have the concept of an array length. Currently we use a free variable `a.length` that should represent the array length. So the full representation of an array becomes `(a, a.null, a.length)`.

Similarly to null arrays you can get interesting results:

```java
public static void test1(int[] a) {
    pre(exists(a, i -> a[i + 1] > a[i]));
	post(true);
}

public static void test2(int[] a) {
	pre(false);
    //pre(exists(a, i -> a[i+1] >= a[i]));
    post(true);
}
```

The resulting (raw) model:

```
a.length -> #x00000001
i!0 -> #x00000000
a -> (_ as-array k!1)
k!1 -> {
  #x00000000 -> #x00000000
  #x00000001 -> #x00000001
  else -> #x00000000
}
```

This model tells us that `a.length == 1` and that `a == [0, 1]` which is contradictory. A postprocessing step has been added to pretty print the model. And the result will instead show:

```
a = inconsistent array representation
```

#### Bit vectors

> Modern CPUs and main-stream programming languages use arithmetic over fixed-size bit-vectors. The theory of bit-vectors allows modeling the precise semantics of unsigned and of signed two-complements arithmetic. There are a large number of supported functions and relations over bit-vectors.
> 
> https://rise4fun.com/z3/tutorialcontent/guide#h25

To implement the semantics of Java (and C#, C++, etc) we use Z3 bit vectors to represent integers. Things like overflow semantics are then handled correctly by Z3.

#### Type checking and type limitations

Currently the implementation only supports the following types:

- `int`
- `bool`
- `int[]`
- `bool[]`

Adding additional types (like various integer sizes, `float`) would require some kind of type analysis, which is possible, but would take a considerable amount of time to implement. It would also require the model parser to be updated to support the constructs.

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

A crude parser has been implemented with [alex and happy](https://leanpub.com/alexandhappy/read). It handles only the observed model cases since no documentation appears to exist on what exactly a Z3 model is. The model data structure is rather simple and can be found in `src/ModelParser/Model.hs`.

#### Side effects

Expressions like `i++ > 0 && i > 1` are not supported because they are considered out of scope for this project.

#### Relevant resources

- https://git.science.uu.nl/d.h.ogilvie2/javawlp/wikis/Resources-about-z3
- https://git.science.uu.nl/d.h.ogilvie2/javawlp/wikis/home

## TODO

- TODO: document usage of haskell code
- TODO: document haskell code itself better
- TODO: latex (https://www.overleaf.com/11599874kpwfhwctdqkr#/43883318/)
