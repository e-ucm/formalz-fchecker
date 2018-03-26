#!/bin/bash

# Get latest HEAD
git pull origin master
# Build project
stack build
# Kill previous process
immortalctl halt javawlp
# Immortal will automatically restart the new process
