# zhalisp - C++ implementation
This directory contains a C++ implementation of zhalisp

## Compilation
* Use `make` to generate the simplest version. 
* Use `make ALL=1 O=3` to enable all features (explained below) and
use highest optimization.

### Compile time arguments and macros
* Adding `TR=1` in `make` enables macro `TAIL_RECU`, which enables tail recursion optimization.
* Adding `LS=1` in `make` enables macro `LIMIT_STACK`, which enables and sets default stack limit.
	* The value can be changed using function `SETSTACKLIMIT`.
* Adding `O=1`, `O=2`, or `O=3` in `make` controls optimization level. 
* Adding `EP=1` in `make` enables macro `REMOVE_EXP_POS`, which removes the plus sign after 'E' in floating point number's scientific notation.
* Adding `PTR=1` in `make` enables macro `CUSTOM_PTR` enables trace-based garbage collection.
	* Use `DEBUG_SPTR` to print debug information like construct / destruct info (need to change code).

## Usage
* Execute `backend` with no arguments will result in shell mode. Type an s-expression and press Enter. 
* Execute `backend` with test file names (ends with `.test`) as arguments will perform tests with the test files. `-` (meaning stdin) will open shell mode. 

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
	* Arithmetic uses GMP
* `lex.l`: Lex specification for the grammar
* `translate.y`: Yacc specification for the grammar
* `evaluate.cpp`: backend
	* `PTR<Sexp> evaluate(PTR<Sexp>, ENV)` evaluates any s-expression
* `test.cpp`: function to run test cases / make shell interactions
* `backend.cpp`: file containing the `main` function

## Limitations
* `eq` may have different behavior than Clisp (currently same as `eql`)
* `typep` not implemented fully
* `prog` may have incorrect behavior (due to `go` and `return` statements)
* Memory leak
	* If macro `CUSTOM_PTR` is not enabled, there will be memory leak due to reference cycles between Env and Func objects
	* If it is enabled
		* The garbage is only collected after each line of interactive mode / test mode completes
		* This will cause an nonnegligible overhead on computation time
* The scope for `eval` is incorrect.
	* e.g. `(setq a 0) (let ((a 1)) (eval 'a))` should give 0, but gives 1. 

## References
* The GNU Multiple Precision Arithmetic Library (GMP)
	* https://gmplib.org/manual/index.html
	* `#include <gmpxx.h>`, `-lgmp -lgmpxx`
* [(An ((Even Better) Lisp) Interpreter (in Python))](http://norvig.com/lispy2.html) by Peter Norvig

## Insights
* Major development time: 9.5 days
	* Greatly referring to the Python 3 implementation
* Time to compile (at git [257f6b3](https://github.com/lxylxy123456/zhalisp/commit/257f6b3abec4969f9c33c5645bd0a825139661b4)): 11.225 (average of 10 tries)
* Time to run all tests (at git 257f6b3): 0.060s (average of 10 tries)
* Memory leak when running all tests (at git 257f6b3)
	* definitely lost: 1,296 bytes in 9 blocks
	* indirectly lost: 91,876 bytes in 2,782 blocks
* About 2800 lines of code

## Insights updated
* Major development time: 28 days
* Time to compile (O3 optimization, at git [3925b1](https://github.com/lxylxy123456/zhalisp/commit/3925b1b6062bb5685bdf7765211b95d931efab15)): 20.559s
* Time to run all tests (test data at git 257f6b3): 0.145s
	* if `LIMIT_STACK` is turned off: 0.032s
* No memory leak after running all tests (test data at git 257f6b3)
* About 3800 lines of code

