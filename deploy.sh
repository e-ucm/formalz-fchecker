#!/bin/bash
# $1: port number to run the server
# $2: filename to log output

PORT="${1:-8080}"
LOG="${2:-out.log}"
LOGPATH="$(pwd)/$LOG"
BACKUPPATH="$(pwd)/$LOG".backup

# Get latest HEAD
git pull origin master
# Build project
stack build
# Kill previous process
pkill javawlp
# Backup previous log if it exists by appending it to a backup file.
if [ -f "$LOGPATH" ]
then
	cat "$LOGPATH" >> "$BACKUPPATH"
	rm "$LOGPATH"
fi
# Run server
nohup stack exec javawlp -- --runServer -p $PORT > $LOG &