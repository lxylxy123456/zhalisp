# zhalisp
A "zha" Clisp interpreter implementation
* Zha ([Wiktionary](https://en.wiktionary.org/wiki/%E6%AE%98%E6%B8%A3#Noun)) here means not efficient / limited / bad design. 

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

## Implemented Functions

| Lisp		| Python	| Sample							| Result					| Category				|
|-----------|-----------|-----------------------------------|---------------------------|-----------------------|
|`+`		|`plus`		|`(+ 1 2 3)`						|`6`						| Arithmetics			|
|`-`		|`minus`	|`(- 1 2)` <br> `(- 1)`				|`-1` <br> `-1`				|						|
|`*`		|`mul`		|`(* 1 2 3)`						|`6`						|						|
|`/`		|`div`		|`(/ 1 2)`							|`1/2`						|						|
|`1+`		|`one_plus`	|`(1+ 2)`							|`3`						|						|
|`=`		|`eq_`		|`(= 1 2)`							|`NIL`						|						|
|`<`		|`lt`		|`(< 1 2)`							|`T`						|						|
|`<=`		|`le`		|`(<= 1 2)`							|`T`						|						|
|`>`		|`gt`		|`(> 1 2)`							|`NIL`						|						|
|`>=`		|`ge`		|`(>= 1 2)`							|`NIL`						|						|
|`NULL`		|`null`		|`(NULL NIL)` <br> `(NULL T)`		|`T` <br> `NIL`				| Unary Predicates		|
|`ATOM`		|`atom`		|`(ATOM NIL)` <br> `(ATOM '(1))`	|`T` <br> `NIL`				|						|
|`LISTP`	|`listp`	|`(LISTP 1)` <br> `(LISTP NIL)`		|`T` <br> `NIL`				|						|
|`ZEROP`	|`zerop`	|`(ZEROP 0)` <br> `(ZEROP 1)`		|`T` <br> `NIL`				|						|
|`EQ`		|`eq`		|`(EQ 0 1)`							|`NIL`						| Binary Predicates		|
|`EQL`		|`eql`		|`(EQL 0 1)`						|`NIL`						|						|
|`EQUAL`	|`equal`	|`(EQUAL '(0 1) '(0 1))`			|`T`						|						|
|`AND`		|`and_`		|`(AND T NIL)`						|`NIL`						| Logic					|
|`OR`		|`or_`		|`(OR T NIL)`						|`T`						|						|
|`NOT`		|`not_`		|`(NOT NIL)` <br> `(NOT 0)`			|`T` <br> `NIL`				|						|
|`CAR`		|`car`		|`(CAR '(0 1 2))`					|`0`						| List operations		|
|`CDR`		|`cdr`		|`(CDR '(0 1 2))`					|`(1 2)`					|						|
|`CONS`		|`cons`		|`(CONS 0 '(1 2))`					|`(0 1 2)`					|						|
|`LIST`		|`list_`	|`(LIST (+ 1 2) (- 3 4))`			|`(3 -1)`					|						|
|`MAPCAR`	|`mapcar`	|`(MAPCAR #'+ '(1 2) '(10 20))`		|`(11 22)`					| High-Order Functions	|
|`MAPC`		|`mapc`		|`(MAPC #'+ '(1 2) '(10 20))`		|`(1 2)`					|						|
|`MAPLIST`	|`maplist`	|`(MAPLIST #'cons '(2 3) '(20 30))` |`(((2 3) 20 30) ((3) 30))`	|						|
|`APPEND`	|`append`	|`(APPEND '(1 2) '(3 4) '(5 6))`	|`(1 2 3 4 5 6)`			|						|
|`DEFUN`	|`defun`	|`(DEFUN F (X) (* X X))`			|`F`						| Functions				|
|`APPLY`	|`apply`	|`(APPLY #'+ '(1 2 3))`				|`6`						|						|
|`FUNCALL`	|`funcall`	|`(FUNCALL #'+ 1 2 3)`				|`6`						|						|
|`FUNCTION`	|`function`	|`(FUNCALL (FUNCTION +) 1 2 3)`		|`6`						|						|
|`QUOTE`	|`quote`	|`(QUOTE (1 2 3))`					|`(1 2 3)`					|						|
|`EVAL`		|`eval_`	|`(EVAL '(+ 1 2))`					|`(3)`						| Variables				|
|`LET`		|`let`		|`(LET ((X 1)) X)`					|`1`						|						|
|`LET*`		|`let_star`	|`(LET* ((X 1) (Y X)) Y)`			|`1`						|						|
|`SETQ`		|`setq`		|`(SETQ X 1)`						|`X`						|						|
|`COND`		|`cond`		|`(COND (nil 1) (t 2))`				|`2`						| Conditions			|

