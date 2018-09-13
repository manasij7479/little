# little

# Building
```
$ mkdir build && cd build
$ cmake ../
$ make
```

# Running examples

For now, this terminates with a 0 status code if the input is parsed.

```
$ ./build/driver/little examples/bubblesort.lil
```

To print out the AST:
```
$ ./build/driver/little examples/bubblesort.lil --print-ast
```
