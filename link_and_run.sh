#!/bin/bash
make -j9
./driver/little $1 --print-ir > /tmp/foo.ll
clang -Wno-override-module ../runtime/littlert.c /tmp/foo.ll -O2 -o /tmp/foo.out
/tmp/foo.out
