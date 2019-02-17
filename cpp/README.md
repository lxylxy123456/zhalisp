# zhalisp - C++ implementation
This directory contains a C++ implementation of zhalisp

## Usage
* Execute `backend` with no arguments will result in shell mode. Type an s-expression and press Enter. 
* Execute `tests` with test file names (ends with `.test`) as arguments will perform tests with the test files. 

## Project Structure
* `structs.h`: all structures used, which are included in `structs/`
* `lex.l`: Lex specification for the grammar
* `translate.y`: Yacc specification for the grammar
* `evaluate.cpp`: backend
	* `PTR<Sexp> evaluate(PTR<Sexp>, ENV)` evaluates any s-expression
* `test.cpp`: run test cases
* `backend.cpp`: interactive shell

## TODO
* How to format float output? [https://stackoverflow.com/questions/18832856/](https://stackoverflow.com/questions/18832856/)

## References
* The GNU Multiple Precision Arithmetic Library (GMP)
	* https://gmplib.org/manual/index.html
	* `#include <gmpxx.h>`, `-lgmp -lgmpxx`

## Insights
* Major development time: 9.5 days
	* Greatly referring to the Python 3 implementation
* Time to run all tests (at git [257f6b3](https://github.com/lxylxy123456/zhalisp/commit/257f6b3abec4969f9c33c5645bd0a825139661b4)): 0.060s (average of 10 tries)
* Time to compile (at git [257f6b3](https://github.com/lxylxy123456/zhalisp/commit/257f6b3abec4969f9c33c5645bd0a825139661b4)): 11.225 (average of 10 tries)
* About 2800 lines of code

