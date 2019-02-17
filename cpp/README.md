# zhalisp - C++ implementation
This directory contains a C++ implementation of zhalisp

## Usage
* Execute `backend` with no arguments will result in shell mode. Type an s-expression and press Enter. 
* Execute `tests` with test file names (ends with `.test`) as arguments will perform tests with the test files. 

### Example
```
$ ./backend 
-> (defun factorial (n) (cond ((zerop n) 1) (t (* n (factorial (1- n))))))
=> FACTORIAL
-> (factorial 20)
=> 2432902008176640000
-> (defun fib (n)
    (cond ((or (equal n 1) (equal n 2)) 1) (t (+ (fib (1- n)) (fib (- n 2))))))
=> FIB
-> (fib 7)
=> 13
-> CLEAR-ENV
=> CLEAR-ENV
-> (fib 7)
Error: Function not found
-> (exit)
$ 
```

## Project Structure
* `structs.h`: all structures used, which are included in `structs/`
* `lex.l`: Lex specification for the grammar
* `translate.y`: Yacc specification for the grammar
* `evaluate.cpp`: backend
	* `PTR<Sexp> evaluate(PTR<Sexp>, ENV)` evaluates any s-expression
* `test.cpp`: run test cases
* `backend.cpp`: interactive shell

## Limitations
* `eq` may have different behavior than Clisp (currently same as `eql`)
* `typep` not implemented fully
* `prog` may have incorrect behavior (due to `go` and `return` statements)
* Memory leak (reason: reference cycles between Env and Func objects)

## TODO
* How to format float output? [https://stackoverflow.com/questions/18832856/](https://stackoverflow.com/questions/18832856/)

## References
* The GNU Multiple Precision Arithmetic Library (GMP)
	* https://gmplib.org/manual/index.html
	* `#include <gmpxx.h>`, `-lgmp -lgmpxx`

## Insights
* Major development time: 9.5 days
	* Greatly referring to the Python 3 implementation
* Time to compile (at git [257f6b3](https://github.com/lxylxy123456/zhalisp/commit/257f6b3abec4969f9c33c5645bd0a825139661b4)): 11.225 (average of 10 tries)
* Time to run all tests (at git 257f6b3): 0.060s (average of 10 tries)
* Memory leak when running all tests (at git 257f6b3)
	* definitely lost: 1,296 bytes in 9 blocks
	* indirectly lost: 91,876 bytes in 2,782 blocks
* About 2800 lines of code

