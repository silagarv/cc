# CC - C Frontend

## Overview
This is my currently work in process, c compiler. This was started as a personal intrest after taking a compilers course at uni and I have been working on this for a few months now.
It is written in C99 and the goal is to eventually be able to parse all c99 code (including hopefully itself). The project relies on LLVM to generate code in order to hopefully learm more about it.

Currently, the program is able to read, lex, and parse most typical c99 constructs and does most ast building and creation and has the starts of code generation to LLVM IR. However, initialization is almost not implemented at all apart from some basic checking.

Currently, the the command line driver is pretty limited where is can parse some warning options but ignores them.

This project take inspiration from the clang compiler and aims to be as correct as possible with helpful error messages.

## Building
Requirements
- C99 compiler
- LLVM
- make

To building, run 'make' in the cc directory and it wll output an executable named 'cc'

## Current TODO
- Declaration parsing
    - also include a second list in struct / union decl's for all decls inside it
    - Fix declarations groups so they also optionally include a struct / unions definition inside them if it was
    defined at that point. will have to go fix up for loop checking after this however
    - Need to fix non-detected of [*] modifier in function definitions

- Semantic
    - expressions
        - fix array subscript checking code -> register variable error not triggered in some cases.
        - Create code for casting when folding constant expressions
        - Fix bug for calculating sizeof 0 sized types and making arrays with them
    - initializers 
        - clean up the code for variable initializations to make it alot more clean so that it can easily be seen what is happening where.
        - also make the calculation of what is tentative and what is not tentative more clear so that we can do the above better
    - Finish proper switch statement building

- Diagnostic
    - Clean up file diagnostic.c and consolidate how diagnostics are emitted to be a much cleaner interface.

## Later TODO
- Fully featured command-line driver
- Preprocessor support
- Multiple target support
