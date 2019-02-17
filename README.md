# zhalisp
A "zha" Clisp interpreter implementation, written in Python 3
* Zha ([Wiktionary](https://en.wiktionary.org/wiki/%E6%AE%98%E6%B8%A3#Noun)) here means not efficient / limited / bad design. 

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
* `typep` does not implement fully
* `prog` may have incorrect behavior (due to `go` and `return` statements)
* `symbol-function` has difficulties to be implemented
* Cannot do recursion with too many levels (has `RecursionError`)
	* The limit can be increased using `SETRECURSIONLIMIT`, but may have Segmentation fault

## Implemented Functions

| Lisp		| Python	| Sample							| Result					| Category				|
|-----------|-----------|-----------------------------------|---------------------------|-----------------------|
|`+`		|`plus`		|`(+ 1 2 3)`						|`6`						| Arithmetics			|
|`-`		|`minus`	|`(- 1 2)` <br> `(- 1)`				|`-1` <br> `-1`				|						|
|`*`		|`mul`		|`(* 1 2 3)`						|`6`						|						|
|`/`		|`div`		|`(/ 1 2)`							|`1/2`						|						|
|`1+`		|`one_plus`	|`(1+ 2)`							|`3`						|						|
|`1-`		|`one_minus`|`(1- 2)`							|`1`						|						|
|`=`		|`eq_`		|`(= 1 2)`							|`NIL`						|						|
|`<`		|`lt`		|`(< 1 2)`							|`T`						|						|
|`<=`		|`le`		|`(<= 1 2)`							|`T`						|						|
|`>`		|`gt`		|`(> 1 2)`							|`NIL`						|						|
|`>=`		|`ge`		|`(>= 1 2)`							|`NIL`						|						|
|`SQRT`		|`sqrt_`	|`(SQRT 4)`							|`2.0`						|						|
|`ATOM`		|`atom`		|`(ATOM NIL)` <br> `(ATOM '(1))`	|`T` <br> `NIL`				| Unary Predicates		|
|`LISTP`	|`listp`	|`(LISTP 1)` <br> `(LISTP NIL)`		|`T` <br> `NIL`				|						|
|`NULL`		|`null`		|`(NULL NIL)` <br> `(NULL T)`		|`T` <br> `NIL`				|						|
|`NUMBERP`	|`numberp`	|`(NUMBERP 0)` <br> `(NUMBERP ())`	|`T` <br> `NIL`				|						|
|`TYPEP`	|`typep`	|`(TYPEP 0 'NUMBER)`				|`T`						|						|
|`SYMBOLP`	|`symbolp`	|`(SYMBOLP 'A)` <br> `(SYMBOLP 1)`	|`T` <br> `NIL`				|						|
|`ZEROP`	|`zerop`	|`(ZEROP 0)` <br> `(ZEROP 1)`		|`T` <br> `NIL`				|						|
|`EVENP`	|`evenp`	|`(EVENP 0)` <br> `(EVENP 1)`		|`T` <br> `NIL`				|						|
|`ODDP`		|`oddp`		|`(ODDP 0)` <br> `(ODDP 1)`			|`NIL` <br> `T`				|						|
|`EQ`		|`eq`		|`(EQ 0 1)`							|`NIL`						| Binary Predicates		|
|`EQL`		|`eql`		|`(EQL 0 1)`						|`NIL`						|						|
|`EQUAL`	|`equal`	|`(EQUAL '(0 1) '(0 1))`			|`T`						|						|
|`AND`		|`and_`		|`(AND T NIL)`						|`NIL`						| Logic					|
|`OR`		|`or_`		|`(OR T NIL)`						|`T`						|						|
|`NOT`		|`not_`		|`(NOT NIL)` <br> `(NOT 0)`			|`T` <br> `NIL`				|						|
|`CAR`		|`car`		|`(CAR '(0 1 2))`					|`0`						| List operations		|
|`CDR`		|`cdr`		|`(CDR '(0 1 2))`					|`(1 2)`					|						|
|`CONS`		|`cons`		|`(CONS 0 '(1 2))`					|`(0 1 2)`					|						|
|`MEMBER`	|`member`	|`(MEMBER '1 '(0 1 2))`				|`(1 2)`					|						|
|`LIST`		|`list_`	|`(LIST (+ 1 2) (- 3 4))`			|`(3 -1)`					|						|
|`MAPCAR`	|`mapcar`	|`(MAPCAR #'+ '(1 2) '(10 20))`		|`(11 22)`					| High-Order Functions	|
|`MAPC`		|`mapc`		|`(MAPC #'+ '(1 2) '(10 20))`		|`(1 2)`					|						|
|`MAPLIST`	|`maplist`	|`(MAPLIST #'cons '(2 3) '(20 30))` |`(((2 3) 20 30) ((3) 30))`	|						|
|`APPEND`	|`append`	|`(APPEND '(1 2) '(3 4) '(5 6))`	|`(1 2 3 4 5 6)`			|						|
|`DEFUN`	|`defun`	|`(DEFUN F (X) (* X X))`			|`F`						| Functions				|
|`LAMBDA`	|`lambda_`	|`((LAMBDA (X) (* X X X)) 10)`		|`1000`						|						|
|`APPLY`	|`apply`	|`(APPLY #'+ '(1 2 3))`				|`6`						|						|
|`FUNCALL`	|`funcall`	|`(FUNCALL #'+ 1 2 3)`				|`6`						|						|
|`FUNCTION`	|`function`	|`(FUNCALL (FUNCTION +) 1 2 3)`		|`6`						|						|
|`QUOTE`	|`quote`	|`(QUOTE (1 2 3))`					|`(1 2 3)`					|						|
|`EVAL`		|`eval_`	|`(EVAL '(+ 1 2))`					|`(3)`						|						|
|`LET`		|`let`		|`(LET ((X 1)) X)`					|`1`						| Variables				|
|`LET*`		|`let_star`	|`(LET* ((X 1) (Y X)) Y)`			|`1`						|						|
|`SETQ`		|`setq`		|`(SETQ X 1)` <br> `X`				|`1` <br> `1`				|						|
|`SET`		|`set_`		|`(SET 'X 1)` <br> `X`				|`1` <br> `1`				|						|
|`COND`		|`cond`		|`(COND (nil 1) (t 2))`				|`2`						| Conditions			|
|`IF`		|`if_`		|`(IF (> 1 2) 1 2)`					|`2`						| 						|
|`DO`		|`do_`		|`(DO () (T 1))`					|`1`						| Iteration				|
|`PROG`		|`prog`		|`(PROG ((X 1)) (RETURN X))`		|`1`						| 						|
|`PRINT`	|`print_`	|`(PRINT 2)`						|`2`						| I/O					|

## Special functions
* `CLEAR-ENV` clears current environment (all variables and functions)
* `(EXIT)` exits interactive mode (cannot be nested function call)
* `(SETRECURSIONLIMIT 1000)` sets Python [recursion limit](https://docs.python.org/3/library/sys.html#sys.setrecursionlimit)

### To be implemented
* string
* complex
* `load` (P51)
* allow built-in functions written in Lisp

