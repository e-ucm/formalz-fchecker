# javawlp Library

javawlp is a Haskell library that implements Dijkstra's weakest pre-condition transformer that targets the Java programming language. 
Given a statement *stmt* and a post-condition *q*, the transformer *wlp stmt q* calculates the weakest pre-condition for the statement
to establish the post-condition q. Originally, the transformer is used in the context of a programming logic:
to prove that a Hoare triple specification *{p} stmt {q}* is valid we can instead prove that the
implication *p implies (wlp stmt q)* is valid. Over the years, predicate transformers are also used for other purposes,
such as to aid software testing.

The weakest pre-condition of loops and recursions is unfortunately in general uncomputable. javawlp takes the pragmatic approach
to unroll loops and recursions some finite number of times (configurable). As such, verification through javawlp falls
in the category bounded verification.

Copyright (c) 2017, Utrecht University

Authors: Koen Wermer, Wishnu Prasetya

License: GPL

### How to deploy

TO DO: Provide a cabal file.

javawlp is distributed as a Haskell library. The main functions exported by the library can be found in the module `Javawlp.API`.

Example of use:

```
module Mymodule where
import Javawlp.API
test = do -- parse a Java source file; specify the path to the file:
          (tyenv,decls,methods) <- parseJava filepath  
          configuration <- defaultConf
          -- specify a post-condition and calculate the wlp:
          p <- wlpMethod configuration tyenv decls (Ident methodname) (post_ "returnValue == 0")
          -- do something with the obtained wlp (p):
          putStrLn (prettyPrint p)
```

You can check the obtained pre-condition for its satisfiability and obtained the value assignment that
satisfies it as shown below. This is for example useful to obtain a test case. 

```
module Mymodule where
import Javawlp.API
import Z3.Monad
import Javawlp.Engine.Verifier
import Javawlp.Engine.HelperFunctions
test = do -- this part is as in the previous example:
          (tyenv,decls,methods) <- parseJava filepath  
          configuration <- defaultConf
          p <- wlpMethod configuration tyenv decls (Ident methodname) (post_ "returnValue == 0")
          -- now check p's satisfiability and print its model: 
          let (result,model) = unsafeIsSatisfiable (extendEnv tyenv decls (Ident methodname)) decls p
          case result of
             Sat   -> do 
                      let (Just m) = model
                      s <- evalZ3 (modelToString m)
                      putStrLn ("SATISFIABLE. Model: " ++ s)
             Unsat -> putStrLn "Not satisfiable."
             _     -> putStrLn "Unable to decide."  
```

### Content

- src: contains javawlp's source files.
- papers: includes Wermer's original thesis. 
- examples: includes a set of small Java examples you can try as targets for the wlp transformer.

### Dependency

Haskell packages:

- language-java 
- z3 (this is not the actual z3, but provides interface to z3)

Other software:

- You will need the actual z3

### Pre-processing 

Some limitations in the current implementation requires the target source code to meet certain 
syntactical constraints; so some pre-processing is required before a source file can be exposed
to javawlp.

- Reference to an instance member x should be explicitly written as this.x

### IMPRESS EDSL

TODO: Write more information.

#### LogicIR

![](https://i.imgur.com/fBUDkGY.png)