## Top-level structure

The files are organised as follows:

- `app`: The main entry point (handles cmd-line arguments and runs server)
- `impress_edsl`: Example Java project using our EDSL
- `immortal`: Setup for automatically restarting server (see `docs/DEPLOY.md`)
- `docs`: Several documentation files
  - `API.md`: Describes our web API
  - `DEPLOY.md`: Describes the necessary setup to deploy the server
  - `EDSL.md`: Specification of our Java EDSL
  - `STRUCTURE.md`: Describes the file structure of the repo
- `src`: Haskell source files
  - `JavaHelpers`: Several Java-specific utilities, augmenting `language-java`
  - `LogicIR`:
    - `Backend`: Backend implementations for checking semantic equivalence
      - `Z3`: Backend that uses SMT-solving, specifically `Z3`
      - `QuickCheck`: Backend that uses random testing
    - `Frontend`: Interface between the surface and the intermediate language
      - `Java.hs`: Java frontend
    - `Expr.hs`: Basic datatypes of the logic intermediate language
    - `Fold.hs`: Fold algebras over the LogicIR datatypes
    - `Parser.hs`: Parsec parser for LogicIR
    - `ParserUtils.hs`: Several parsec utilities
    - `TypeChecker.hs`: Type-checking for LogicIR
    - `Normalizer.hs`: Fixpoint rewrites of Java/LogicIR expressions
    - `Preprocess.hs`: Pre-processing steps for LogicIR
    - `Pretty.hs`: Pretty-printing of LogicIR expressions
    - `Eval.hs`: Evaluates (constant) LogicIR expressions
  - `API.hs`: Main API which compares two specs using both backends
  - `Model.hs`: Datatypes for model and feedback responses.
  - `Server`: Servant implementation of our web API
- `test`: Test suite for the specification checker
  - `Spec.hs`: Entry point for test suite
  - `TEquivalenceClasses`: Exhaustive pairwise tests (see `test/test_files`)
- `deploy.sh`: Deployment script
- `install_z3.sh`: Z3 installation script
