#!/bin/bash
cd server
echo "stack version"
stack --version
echo "setting up stack"
stack setup
stack build
tail -f /dev/null
# stack exec run-server

