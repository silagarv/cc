#ifndef CODEGEN_DECLARATION_H
#define CODEGEN_DECLARATION_H

#include "parse/declaration.h"

void codegen_declaration(const Declaration* declaration);
void codegen_external_declaration(const Declaration* declaration);

#endif /* CODEGEN_DECLARATION_H */
