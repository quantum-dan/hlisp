# HLisp
A very simple interpreter for a Lisp-type language.

## Building
The only dependency is a Haskell compiler.  Just clone the project and compile Main.hs, presumably to `hlisp`.

Example:
```bash
$ git clone https://github.com/quantum-dan/hlisp
$ cd lisp
$ ghc -o hlisp Main.hs
```

## Usage
* `hlisp -h`: Show help
* `hlisp <filename>`: Execute the contents of the given file
* `hlisp -e <code>`: Execute the given code
* `hlisp -l <filename>`: Load the file and enter the REPL
* `hlisp`: Enter the REPL

## Syntax
HLisp uses the standard Lisp syntax of parenthesis-delimited lists with items separated by spaces.

* Variable definition: `(def <varname> <value>)`
* Lambdas: `(lambda ([argument] [argument] [etc] ) <body>)` (note: functions do not currently support variable-length argument lists)
* Function calls: `(<function> [argument] [argument] [etc])`
* Comments: single-line comments start with `;`
* Symbols: start with `'`, no spaces
* Strings, floats, integers: the usual approach
* Booleans: `#t` and `#f`

The file demo.hl contains examples of all the syntax.

### Primitive Functions
As few primitives as possible to reasonably support functionality are defined.

The following primitive functions are defined:
* `+`: sums the arguments (at least two, variable-length); if both integers and floats are summed, it will cast integers to floats
* `*`: multiplies the arguments (at least two, variable-length); if integers and floats are multiplied, it will cast integers to floats
* `negate`: negates a single number
* `invert`: inverts a single number
* `cons`: makes two arguments into a pair; if applied to more than two arguments, it will make them into nested pairs, with the final argument paired with `unit`, making a linked list
* `car`: first element of a pair
* `cdr`: second element of a pair
* `=`: tests the arguments for equality (variable-length)
* `if`: if first argument, then second argument, else third argument
