# zhalisp
A "zha" Clisp interpreter implementation
* Zha ([Wiktionary](https://en.wiktionary.org/wiki/%E6%AE%98%E6%B8%A3#Noun)) here means not efficient / bad design. 

## Project Structure
* `backend.py`: connecting the interpreter parts together, actually doing nothing
	* `eval_str(str, env)` converts a string to an s-expression and evaluate
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

