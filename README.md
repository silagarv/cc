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
- Driver
    - Make the driver more friendly to being extended so that we are more easily able to extend it to add args as we want and do have to do silly things
    - Maybe create an argparser and use that for parsing???
- Preprocessor
    - Improve diagnostics by giving macro expanded tokens a sensible location (currently it defaults to the location of the macro token)
    - Eventually implement the 'multiple include' optimisation
    - Preprocessor should probably properly propogate spacing information
    - Fix the expresison parser
    - Fix stringification with a final '\'
- Parser & semantic
    - 12E+f -> should trigger an error but doesn't
    - Fix every occurance of getting an identifier from a token so that they use the token_get_identifier instead of getting directly from the token
- Declaration parsing
    - also include a second list in struct / union decl's for all decls inside it as currently the canonical decl is used even in situations where it is not the current decl. Also the lcoations for the decls can be quite wrong as a result.
    - Fix declarations groups so they also optionally include a struct / unions definition inside them if it was
    defined at that point. will have to go fix up for loop checking after this however
    - Need to fix non-detected of [*] modifier in function definitions
- Codegen
    - There is currently a use after free in the code generation for some unknown reason
    - Codegen is also highly incomplete: should I even do this part?

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
- Multiple target support
- Make sure vec doesn't allocate when given a 0 size start
