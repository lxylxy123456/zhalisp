# zhalisp - Python 3 implementation
This directory contains a Python 3 implementation of zhalisp

## Usage
* Execute `backend.py` with no arguments will result in shell mode. Type an s-expression and press Enter. 
* Execute `tests.py` with test file names (ends with `.test`) as arguments will perform tests with the test files. 

### Example
```
$ python3 backend.py
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
Traceback (most recent call last):
  ...
NameError: Cannot find function: FIB
-> (exit)
$ 
```

## Project Structure
* `backend.py`: connecting the interpreter parts together, actually doing nothing
	* `eval_str(str, env)` converts a string to an s-expression and evaluate
	* `shell(env)` implements interactive mode
* `builtin.py`: backend, includes a list of all build-in functions implemented
	* `evaluate(exp, env)` evaluates an s-expression in the given environment
* `frontend.py`: frontend, written in [ply](https://www.dabeaz.com/ply/ply.html)
	* `build_tree(str)` converts a string to an s-expression
	* Does not implement all syntax (including string)
* `structs.py`: define classes that build s-expressions, also class for environments
	* Classes: `Atom`, `Dot`, `Number`, `Symbol`, `Bool`, `List`, `Env`
* `tests.py`: some test cases
	* Use `test()` function
* temporary files that may be generated
	* `__pycache__/`
	* `lextab.py`
	* `parser.out`
	* `parsetab.py`

## Limitations
* Complex numbers currently unsupported
	* If enabled, complex + fraction will have different behavior than Clisp
* `eq` have different behavior than Clisp (currently same as `eql`)
* `typep` not implemented fully
* `prog` may have incorrect behavior (due to `go` and `return` statements)
* `symbol-function` has difficulties to be implemented
* Python cannot do recursion with too many levels (has `RecursionError`)
	* The limit can be increased using `SETRECURSIONLIMIT`, but may raise Segmentation fault
	* Calling the function in C++ version have no effects

### To be implemented
* string
* complex
* `load` (P51)
* allow built-in functions written in Lisp

## Insights
* Major development time: 3.5 days
* Time to run all tests (at git [257f6b3](https://github.com/lxylxy123456/zhalisp/commit/257f6b3abec4969f9c33c5645bd0a825139661b4)): 0.216s (average of 10 tries)
* About 1200 lines of code

