#ifndef COMPOUND_LAYOUT_CALCULATOR_H
#define COMPOUND_LAYOUT_CALCULATOR_H

#include <stddef.h>
#include <stdint.h>

#include "driver/diagnostic.h"

#include "parse/declaration.h"

typedef struct CompoundLayoutEntry {
    Declaration* member; // The member this is for

    uint64_t bytes; // the number of bytes of offset we have
    uint64_t bits; // the number of bits of offset we have

    struct CompoundLayoutEntry* next;
} CompoundLayoutEntry;

typedef struct CompoundLayout {
    CompoundLayoutEntry* entries; // all the entries in the layout
    size_t size; // the size of the layout
    size_t alignment; // the alignment of the layout
} CompoundLayout;

size_t compound_layout_get_size(const CompoundLayout* layout);
size_t compound_layout_get_align(const CompoundLayout* layout);
void compound_layout_member_offset(const CompoundLayout* layout,
        const Declaration* member, uint64_t* bytes, uint64_t* bits);

void calculate_compound_layout(AstAllocator* allocator, DiagnosticManager* dm,
        Declaration* declaration);

#endif /* COMPOUND_LAYOUT_CALCULATOR_H */
