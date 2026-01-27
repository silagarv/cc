#ifndef COMPOUND_LAYOUT_CALCULATOR_H
#define COMPOUND_LAYOUT_CALCULATOR_H

#include "driver/diagnostic.h"

#include "parse/declaration.h"

void calculate_compound_layout(DiagnosticManager* dm, Declaration* declaration);

#endif /* COMPOUND_LAYOUT_CALCULATOR_H */
