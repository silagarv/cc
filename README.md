# CC - C Frontend

## Overview
This is my currently work in process, c frontend. This was started as a personal intrest after taking a compilers course at uni and I have been wroking on this for a few months now.
It is written in c99 and the goal is to eventually be able to parse all c99 code (including hopefully itself). The project is self contained with no dependencies, except clang, and
make.

Currently, the program is able to read, lex, and parse most typical c99 constructs and does some ast building and creation. However, declaration handling is not yet complete, but this
is in progress.

Currently, the program will look for a file named 'test.c' in the working directory and then attempt to parse that file, as a command line driver is planned for the future.

This project take inspiration from the clang compiler, particularly in the code for parsing declarations, and in how locations are tracked. It should be noted that no code is copied from
he project and is originally my own work. Additionally, it should be noted that many of the error messages have also been copied directly from clang as well for fimilarity.

## Building
Requirements
- c99 compiler
- make

To building, run 'make' in the cc directory and it wll output an executable named 'cc'

## Current TODO
- Declaration parsing
    - complete the building of the function types (knr type left only)
    - complete struct / union building including sizeof and offsets...

- Semantic
    - expressions
        - fix constant expressions -> some bugs to do with checking if one is a constant expression or not.
        - fold constant expressions -> fix bugs which appear
        - fix array subscript checking code -> register variable error not triggered in some cases.
    - initializers 
        - clean up the code for variable initializations to make it alot more clean so that it can easily be seen what is happening where.
        - also make the calculation of what is tentative and what is not tentative more clear so that we can do the above better
    - Finish proper switch statement building

## Later TODO
- Command-line driver
- Code generation?
- Tuning and naming of warning options
- Printing of source in diagnostics where relavent
- Preprocessor support
