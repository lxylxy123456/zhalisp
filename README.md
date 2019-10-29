# zhalisp - arithmetic operations implemented with bitwise operations

## Build
* `make` to make the executables
* `./backend` to run the executable

## Usage
* Copy functions under `bit_operation.lisp` to a shell
* Use function `plus`, `minus`

## Example
```
-> (plus 3 4)
=> 7
-> (minus 3 4)
=> -1
-> 
```

## Checking
1. Build the project
2. `python3 check.py | ./backend`
3. Make sure that at the end the program prints `=> T`

