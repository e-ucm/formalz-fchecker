#!/bin/bash   

# Script to generate the source code of the mutants, which the wlp needs to do its analysis.
#

# path to Major compiler:
MAJOR="../tools/major/bin/javac"
# Path to major mml compiler
MMLC="../tools/major/bin/mmlc"

# The set and scope of mutations are specified in the file mutation.mml, compile it first:
# $MMLC "./mutation.mml"

mutate(){
    $MAJOR -J-Dmajor.export.directory="./mutants/generated/$1" -J-Dmajor.export.mutants=true -XMutator="./mutation.mml.bin" -cp "./subjects/bin" "./subjects/src/$1.java"
    rm "./subjects/src/$1.class"
}
mutate  "Triangle" 
mutate  "MinsMaxs" 