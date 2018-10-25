#!/bin/bash
mkdir -p build
cd build
cmake ../
make -j9
cp ../runtime/littlert.c .


