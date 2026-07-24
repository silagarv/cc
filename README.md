# CC - C Frontend

## Overview
This is my currently work in process, c compiler frontend. Originally this started as a project to learn more about the workings of the C-preprocessor and how macro expansion works and grew from there.

It is written in C99 and the goal is to eventually be able to parse all C99 code (including hopefully itself), handling any command line flags as needed.

Currently, the program is able to lex, parse, and perform semantic checking on most C99 code whilst constructing an AST and issuing errors and warnings as appropriate. However, aggregate initialization is almost non-implemented apart from some basic checking.

Additionally, many common C compiler extensions like attributes are also unimplemented.

This project take inspiration from Clang and aims to be as correct as possible with helpful error messages.

## Building
Requirements:
1. C99 compiler
2. make

To build, run 'make' and it will output an executable named 'cc'

## Current TODO
- AST
    - Move into the AST subdirectory
- Driver
    - Make the driver more friendly to being extended
- Preprocessor
    - Improve diagnostics by giving macro expanded tokens a sensible location (currently it defaults to the location of the macro token)
    - Implement the multiple include optimization
    - Ensure we correctly propagate spacing information
    - Improve the PP expression parser
    - Fix stringification with a final '\'
- Parser
    - Split up the parser into multiple smaller files
    - 12E+f should trigger an error but does not currently
    - Make coding style across the parser consistent
- Declaration parsing
    - also include a second list in struct / union decl's for all decls inside it as currently the canonical decl is used even in situations where it is not the current decl
    - Fix declarations groups so they also optionally include a struct / unions definition inside them if it was
    defined at that point. Will have to go fix up for loop checking after this however
    - Need to fix non-detected of `[*]` modifier in function definitions

- Semantic
    - Split up into multiple files in its own subdirectory
    - expressions
        - Fix array subscript checking code as register variable error not triggered in some cases.
        - Create code for casting when folding constant expressions
        - Fix bug for calculating `sizeof(T) == 0` sized types and making arrays with them
    - initializers 
        - clean up the code for variable initializations to make it clearer.
        - Make the calculation of what is tentative and what is not tentative more clear so that we can do the above better

- Diagnostic
    - Clean up file `diagnostic.c`
    - consolidate how diagnostics are emitted to be a cleaner interface.
    - Make diagnostics able to print different things e.g. printing of types

