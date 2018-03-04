#!/bin/bash
# $1: port number to run the server
# $2: filename to log output

# Get latest HEAD
git pull origin master
# Build project
stack build
# Kill previous process
pkill javawlp
# Backup previous log
if [ -f $2 ]
then
    mv $2 "$2".backup
fi
 # Run server
nohup stack exec javawlp -- --runServer -p $1 > $2 &
