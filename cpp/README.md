# zhalisp - C++ implementation
This directory contains a C++ implementation of zhalisp

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

## Compile time arguments and macros
* Macro `TAIL_RECU` enables tail recursion optimization (`TR_FLAG` in Makefile)
* Macro `LIMIT_STACK` enables and sets default stack limit (`LS_FLAG` in Makefile)
	* The value can be changed using `SETSTACKLIMIT`
* `O_FLAG` in Makefile controls optimization level
* Macro `REMOVE_EXP_POS` removes the plus sign after 'E' in floating point number's scientific notation (`EXP_POS_FLAG` in Makefile)
* Macro `CUSTOM_PTR` enables trace-based garbage collection (`PTR_FLAG` in Makefile)
	* Use `DEBUG_SPTR` to print debug information like construct / destruct info

## Limitations
* `eq` may have different behavior than Clisp (currently same as `eql`)
* `typep` not implemented fully
* `prog` may have incorrect behavior (due to `go` and `return` statements)
* Memory leak
	* If macro `CUSTOM_PTR` is not enabled, there will be memory leak due to reference cycles between Env and Func objects
	* If it is enabled
		* The garbage is only collected after each line of interactive mode / test mode completes
		* This will cause an nonnegligible overhead on computation time

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

