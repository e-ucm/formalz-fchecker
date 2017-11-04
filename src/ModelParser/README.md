# ModelParser

To generate the parser and lexer run the following commands from this directory:

```
cabal install alex happy
alex Lexer.x
happy Parser.y
```