#ifndef AST_AST_FWD_H
#define AST_AST_FWD_H

// Simple file containing some forward typedef declarations for our ast to 
// reduce having to write e.g. struct ... in some ast header files. This is 
// since the headers we are using currently are quite fragile.

typedef union Type Type;
typedef union Expression Expression;
typedef union Initializer Initializer;
typedef union Statement Statement;
typedef union Declaration Declaration;

#endif /* AST_AST_FWD_H */
