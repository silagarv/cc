#ifndef SYMBOL_H
#define SYMBOL_H

typedef enum SymbolType {
    SYMBOL_UNKNOWN
} SymbolType;

typedef struct SymbolBase {
    SymbolType type;
} SymbolBase;

// functions
// declarations
// definitions???

typedef union Symbol {
    SymbolType type;
    SymbolBase base;
} Symbol;

#endif /* SYMBOL_H */
