# zhalisp
* A "zha" CLISP interpreter implementation
	* Zha ([Wiktionary](https://en.wiktionary.org/wiki/%E6%AE%98%E6%B8%A3#Noun)) here means not efficient / limited / bad design. 
* There are two implementations. Python 3 version in [py](https://github.com/lxylxy123456/zhalisp/tree/master/py) directory, and C++ version in [cpp](https://github.com/lxylxy123456/zhalisp/tree/master/cpp) directory. 

## Implemented Functions

| Lisp		| C++/Python| Sample							| Result					| Category				|
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
|`REDUCE`	|`reduce`	|`(REDUCE #'- '(1 2 3 4))`			|`-8`						|						|
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
	* No effect on C++
* `(SETSTACKLIMIT 8388608)` (if `LIMIT_STACK` is defined) sets C++ stack size (from first call of `evaluate` to somewhere in current function call) to 8 MiB
	* `0` means no limit
	* No effect on Python

