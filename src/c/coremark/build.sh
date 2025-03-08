#!/bin/bash

make PORT_DIR=barebones clean
make PORT_DIR=barebones coremark.exe
