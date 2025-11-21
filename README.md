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
- clang
- make

To building, run 'make' in the cc directory and it wll output an executable named 'cc'

## Current TODO
- Declaration parsing
    - complete the building of the function types
    - complete struct / union parsing and building
    - figure out how we want to store structs / unions once they are fully built

- Scoping
    - Figure out how to go form function prototype scope -> function scope
    - make some fixes for external declarations

- Semantic
    - expressions
    - declarations
    - initializers
    - Finish proper switch statement building

- AST
    - create expressions for all expression types
    - initializer building
    - ast printing (later)

## Later TODO
- Command-line driver
- Code generation?
- Tuning and naming of warning options
- Printing of source in diagnostics where relavent
- Preprocessor support

## Current Known Issues
- Compiles on GCC but will segfault at runtime
