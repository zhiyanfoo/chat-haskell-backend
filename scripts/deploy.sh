#!/bin/bash
cd server
echo "stack version"
stack --version
echo "setting up stack"
stack setup
stack build
echo "starting server"
stack exec run-server

