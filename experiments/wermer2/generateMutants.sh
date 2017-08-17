#!/bin/bash   

# path to Major compiler:
MAJOR="../tools/major/bin/javac"
# Path to major mml compiler
MMLC="../tools/major/bin/mmlc"

# The set and scope of mutations are specified in the file mutation.mml, 
# compile it first:
# $MMLC "./mutation.mml"

mutate(){
    $MAJOR -J-Dmajor.export.directory="./mutants/generated/$1" -J-Dmajor.export.mutants=true -XMutator="./mutation.mml.bin" -cp "./subjects/bin" "./subjects/src/$1.java"
}
mutate  "Triangle" 