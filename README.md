# Mini-C Compiler

This project implements a compiler for a subset of C called Mini-C. Mini-C currently includes the following language features:

### Data types
- integers and characters
- pointers
- arrays

### Control-flow constructs
- for and while loops
- if statements
- functions

Have a look at the `input/fixtures` folder for examples of Mini-C programs.

The code is parsed, typechecked, and compiled into [NASM assembly](https://www.cs.uaf.edu/2017/fall/cs301/reference/x86_64.html) assembly, which can then be assembled by `nasm` or `gcc`.


# Setup
Install the [Cabal package manager](https://www.haskell.org/cabal/) and use it to install the [Happy](https://hackage.haskell.org/package/happy) parser generator for Haskell.
Run `make` afterwards, and a Cabal project will be built automatically, with the compiler executable placed in `output/build/mini-c-compiler/mini-c-compiler`.

# Usage
Run the compiler:
`mini-c-compiler <input_file> <output_file> [--verbose]`

# Contact

Contact me at [mvdcamme@gmail.com](mailto:mvdcamme@gmail.com)