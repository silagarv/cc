# CC - C Frontend

## Overview
This is my currently work in process, c compiler. This was started as a personal intrest and should most definitely not be used for anything.

It is written in C99 and the goal is to eventually be able to parse all c99 code (including hopefully itself). The project relies on LLVM to generate code in order to hopefully learm more about it.

Currently, the program is able to read, lex, and parse most typical c99 constructs and does most ast building and creation. However, initialization is almost not implemented at all apart from some basic checking.

Currently, the the command line driver is pretty limited and most standard C compiler features are no added.

This project take inspiration from the clang compiler and aims to be as correct as possible with helpful error messages.

## Building
Requirements
- C99 compiler
- make

To building, run 'make' in the cc directory and it wll output an executable named 'cc'

## Current TODO
- AST
    - Split up into the AST directory for a better seperation of concerns
- Driver
    - Make the driver more friendly to being extended so that we are more easily able to extend it to add args as we want and do have to do silly things
    - Maybe create an argparser and use that for parsing???
- Preprocessor
    - Improve diagnostics by giving macro expanded tokens a sensible location (currently it defaults to the location of the macro token)
    - Eventually implement the 'multiple include' optimisation
    - Preprocessor should probably properly propogate spacing information
    - Fix the expresison parser
    - Fix stringification with a final '\'
- Parser
    - Split up the parser into multiple smaller files to make it a bit more readable and more modifiable
    - 12E+f -> should trigger an error but doesn't
    - Fix every occurance of getting an identifier from a token so that they use the token_get_identifier instead of getting directly from the token
    - Fix the parsing of numbers and strings so that it gets exact locations now since this is more possible
- Declaration parsing
    - also include a second list in struct / union decl's for all decls inside it as currently the canonical decl is used even in situations where it is not the current decl. Also the lcoations for the decls can be quite wrong as a result.
    - Fix declarations groups so they also optionally include a struct / unions definition inside them if it was
    defined at that point. will have to go fix up for loop checking after this however
    - Need to fix non-detected of [*] modifier in function definitions

- Semantic
    - Split up into multiple files in its own little place as it is unweildly to deal with at the moment
    - expressions
        - fix array subscript checking code -> register variable error not triggered in some cases.
        - Create code for casting when folding constant expressions
        - Fix bug for calculating sizeof 0 sized types and making arrays with them
    - initializers 
        - clean up the code for variable initializations to make it alot more clean so that it can easily be seen what is happening where.
        - also make the calculation of what is tentative and what is not tentative more clear so that we can do the above better

- Diagnostic
    - Clean up file diagnostic.c and consolidate how diagnostics are emitted to be a much cleaner interface.
    - Make diagnostics able to print different things e.g. printing of types would be really helpful to be able to have

## Later TODO
- Fix all of our header naming scheme
- Fully featured command-line driver
- Multiple target support
- Make sure vec doesn't allocate when given a 0 size start
