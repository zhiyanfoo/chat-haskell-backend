#!/bin/bash
cd server
echo "stack version"
stack --version
echo "setting up stack"
stack setup
stack build
stack exec run-server

