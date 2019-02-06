# zhalisp
A "zha" Clisp interpreter implementation
* Zha ([Wiktionary](https://en.wiktionary.org/wiki/%E6%AE%98%E6%B8%A3#Noun)) here means not efficient / bad design. 

## Project Structure
* `backend.py`: connecting the interpreter parts together, actually doing nothing
* `builtin.py`: backend, defining `evaluate` function which evaluates any s-expression
* `frontend.py`: frontend, written by [ply](https://www.dabeaz.com/ply/ply.html)
* `structs.py`: define classes that build s-expressions
* `tests.py`: some test cases
* temporary files that may be generated: `__pycache__/`, `lextab.py`, `parser.out`, `parsetab.py`

