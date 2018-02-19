# IMPRESS EDSL
We want to help students learn about formal program verification. One aspect of this is writing pre and post conditions for their programs. To help the students learn this we developed a tool that can compare two program specifications and can come up with a counter example if the two specifications don't match.

## Dependencies:
- [z3](https://hackage.haskell.org/package/z3):
  ```
  apt-get install z3 libz3-dev
  ```
- [stack](https://docs.haskellstack.org/en/stable/README/):
  ```
  apt-get install stack
  ```

## Usage
To setup the Haskell project:
```
> stack setup
> stack build
```

Command-line usage:
```
stack exec javawlp --
  [--srcA STRING] [--srcB STRING] [-a STRING] [-b STRING]
  [-w|--runServer] [-p|--port INT]
```

To run a comparison between two methods:
```
> stack exec javawlp -- \
  --srcA 'examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java' -a 'real1' \
  --srcB 'examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java' -b 'real2'
```

To run the server API at port 8080:
```
> stack exec javawlp -- -p 8080 --runServer
```

To query the server (see  for available options):
```
> curl -X POST -d <json_object> <server_url>:8080/compare
```

To get the API docs from the server:
- Markdown: Visit `<server_url>:8080/docs`
- Swagger: Visit ``<server_url>:8080/api-swagger`
- [API.md](https://git.science.uu.nl/impresshs/javawlp/blob/master/API.md)

To run the tests:
```
> stack test
```

## Java EDSL
To get started we designed a simple embedded DSL that encapsulates all the expressions taught in the Software Testing & Verification (INFOB3STV) course. This includes:

- Integer/Real expressions (addition, subtraction, multiplication, etc.)
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

### LogicIR.Frontend
Currently there is only one frontend for the Java EDSL, but this could quite easily be extended to other programming languages.

### LogicIR.Backend.Z3
One of the implemented backends is for the [Z3 Theorem Prover](https://github.com/Z3Prover/z3). The `LogicIR.Expr` is converted to a `Z3 AST`.

To determine if the expression `P == Q` is valid, we ask Z3 to prove that `P != Q` is unsatisfiable. There are four possible results:

1. `Sat` -> Z3 proved `P != Q` is satisfiable, which means that the formula `P == Q` is invalid. The Z3 model contains the counter example to provide to the student.
2. `Unsat` -> Z3 proved that `P != Q` is not satisfiable, which means that the formula `P == Q` is valid.
3. `Timeout` -> Z3 did not manage to find a solution in the permitted time interval.
4. `Undef` -> Z3 was unable to decide the satisfiablity of `P != Q`.

In the case of (3) and (4) we have to resort to other methods like QuickCheck to determine if the two formulas are equivalent or not.

#### Notes
One of the assumptions we make is that both specifications are defined in functions that have the same variable and argument names. That way if we have an array `a` that is used in `P`, we know that if `Q` uses `a` that they refer to the same `a`. See [example 1](#example-1) where both specifications have to refer to the `retval`, `oldi` and `oldj` variables in order to allow Z3 to prove anything.

Something to be wary of when reasoning about this is that we are not trying to prove that an individual specification is satisfiable. We are merely interested in proving that two specifications are equal or not. That said, if you ask Z3 if `ForAll(i, i > 0) != ForAll(i, i < 0)` is satisfiable it will give back `Unsat`, because the formula can be reduced to `False != False` which is false. In practice this should be an issue, because the specification that the student's is compared to will be correct.

#### Null arrays
An expression like `a != null` cannot be supported by Z3 directly, because it does not have the concept of null arrays. As a workaround, `a == null` is represented as a free variable `a.null`, so an array essentially becomes a tuple `(a, a.null)`. To achieve this an additional pre-processing step `lExprPreprocessNull` is done after extracting the LogicIR expression.

Because `a` and `a.null` are not bound together by Z3 (accessing a null-array in Java will cause an exception), comparing two methods like this give curious results:
```java
public static void null(int[] a) {
    pre(a == null && a[0] > a[1]);
    post(true);
}

public static void test(int[] a) {
	pre(false);
  post(true);
}
```

The resulting (raw) model:
```scala
a.null -> true
a -> {
  0 -> 1
  1 -> 0
  else -> 1
}
```

Essentially this model tells us that the two preconditions are not equivalent with an `a` that is both `null` and `[1, 0]`. A post-processing step has been added to pretty print the model and it shows:
```scala
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
  post(true);
}
```

The resulting (raw) model:
```scala
a.length -> 1
a -> {
  0 -> 0
  1 -> 1
  else -> 0
}
```

This model tells us that `a.length == 1` and that `a == [0, 1]` which is contradictory. A postprocessing step has been added to pretty print the model. And the result will instead show:
```scala
a = inconsistent array representation
```

#### Type checking and type limitations
Currently the implementation only supports the following types:

- `int`
- `real`
- `bool`
- `int[]`
- `bool[]`
- `real[]`

#### Side effects
Expressions like `i++ > 0 && i > 1` are not supported because they are considered out of scope for this project.

#### Relevant resources
- https://git.science.uu.nl/d.h.ogilvie2/javawlp/wikis/Resources-about-z3
- https://git.science.uu.nl/d.h.ogilvie2/javawlp/wikis/home
