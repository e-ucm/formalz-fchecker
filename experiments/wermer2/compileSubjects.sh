#!/bin/bash  

SRC="./subjects/src"
DESTDIR="./subjects/compiled" 
SUBJECTS="$SRC/Triangle.java $SRC/MinsMaxs.java"
javac -cp "."  -d $DESTDIR $SUBJECTS

