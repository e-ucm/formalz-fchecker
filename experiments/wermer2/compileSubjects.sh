#!/bin/bash  

SRC="./subjects/src"
SUBJECTS="$SRC/Triangle.java"
javac -cp "./subjects/bin"  -d "./subjects/bin" $SUBJECTS

