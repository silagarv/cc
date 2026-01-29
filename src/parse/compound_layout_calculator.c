#include "compound_layout_calculator.h"

#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <string.h>

#include "driver/diagnostic.h"

#include "parse/ast_allocator.h"
#include "parse/type.h"
#include "parse/declaration.h"

#define MAX(a, b) ((a) > (b) ? (a) : (b))

// note: alignment must be a power of two for these to work properly
#define NEED_ROUND_UP(used, align) (((used) % (align)) != 0)
#define ROUND_UP(used, align) (((used) + ((align) - 1)) & ~((align) - 1))

// TODO: should we try to make this more portable
#define BITS_PER_CHAR 8

// TODO: will need to add if we had the packed attribute or not!
typedef struct LayoutCalculator {
    AstAllocator* allocator; // the allocator for out entry layout

    DiagnosticManager* dm; // for emiting any diagnostics that we might need to

    Declaration* decalration; // the declaration we are calculating for

    uint64_t size; // The final size of the compound type
    uint64_t alignment; // the final alignmnt of the compound type

    uint64_t bitfield_offset; // the current bit-offset for the bitfield

    bool is_union; // true if we are a union
    bool padding; // true if we added passing

    CompoundLayoutEntry* first; // the first entry
    CompoundLayoutEntry* recent; // the most recent entry
} LayoutCalculator;

static CompoundLayoutEntry* create_entry(AstAllocator* allocator,
        Declaration* member, uint64_t bytes, uint64_t bits)
{
    CompoundLayoutEntry* entry = ast_allocator_alloc(allocator,
            sizeof(CompoundLayoutEntry));
    entry->member = member;
    entry->bytes = bytes;
    entry->bits = bits;
    entry->next = NULL;
    
    return entry;
}

static void set_next(CompoundLayoutEntry* curr, CompoundLayoutEntry* next)
{
    curr->next = next;
}

static CompoundLayout* create_layout(AstAllocator* allocator, size_t size,
        size_t alignment, CompoundLayoutEntry* entries)
{
    CompoundLayout* layout = ast_allocator_alloc(allocator, size);
    layout->size = size;
    layout->alignment = alignment;
    layout->entries = entries;

    return layout;
}

size_t compound_layout_get_size(const CompoundLayout* layout)
{
    return layout->size;
}

size_t compound_layout_get_align(const CompoundLayout* layout)
{
    return layout->alignment;
}

void compound_layout_member_offset(const CompoundLayout* layout,
        const Declaration* member, uint64_t* bytes, uint64_t* bits)
{
    assert(layout);
    assert(member);

    for (CompoundLayoutEntry* entry = layout->entries; entry != NULL;
            entry = entry->next)
    {
        if (entry->member == member)
        {
            *bytes = entry->bytes;
            *bits = entry->bits;

            return;
        }
    }

    panic("failed to find member in layout!");
}

static void layout_add_entry(LayoutCalculator* state, Declaration* member,
        uint64_t bytes, uint64_t bits)
{
    CompoundLayoutEntry* entry = create_entry(state->allocator, member, bytes,
            bits);
    if (state->first == NULL)
    {
        state->first = entry;
        state->recent = entry;
    }
    else
    {
        set_next(state->recent, entry);
        state->recent = entry;
    }
}

static void calculate_field_layout(LayoutCalculator* state, Declaration* member)
{
    assert(declaration_is(member, DECLARATION_FIELD));

    QualifiedType type = declaration_get_type(member);
    QualifiedType real_type = qualified_type_get_canonical(&type);

    // Get if we are a flexible array member
    bool flex_array = declaration_field_is_fexible_array(member);

    // Get the base size and alignment for the type.
    uint64_t type_size = !flex_array ? qualified_type_get_size(&real_type) : 0;
    uint64_t type_align = qualified_type_get_align(&real_type);

    // Alignment is of strictest aligned member
    state->alignment = MAX(type_align, state->alignment);
    
    if (state->is_union)
    {
        // Union size is the size of the biggest member
        state->size = MAX(type_size, state->size);
        // printf("offset is: 0\n");

        layout_add_entry(state, member, 0, 0);
    }
    else if (declaration_field_has_bitfield(member))
    {
        // TODO: will need to check the portability for this, and also make
        // TODO: sure that it is correct according to the ABI.

        // First start by getting the bitfield size.
        uint64_t bitfield_size = declaration_field_get_bitfield(member);

        // Special case of bitfields that are 0 width (anonymous)
        if (bitfield_size == 0)
        {
            // Only increase the size if the bitfield offset is not 0
            if (state->bitfield_offset != 0)
            {
                state->bitfield_offset = 0;
                state->size++;
                state->padding = true;
            }

            if (NEED_ROUND_UP(state->size, type_align))
            {
                state->size = ROUND_UP(state->size, type_align);
                state->padding = true;
            }
            // printf("offset is: %zu, and %zu bits\n", state->size,
            //     state->bitfield_offset);

            layout_add_entry(state, member, state->size, 0);
        }
        else
        {
            // printf("offset is: %zu, and %zu bits\n", state->size,
            //     state->bitfield_offset);
            layout_add_entry(state, member, state->size,
                    state->bitfield_offset);

            uint64_t post_offset = state->bitfield_offset + bitfield_size;

            state->size += post_offset / BITS_PER_CHAR;
            state->bitfield_offset = post_offset % BITS_PER_CHAR;
        }
    }
    else
    {
        // If this member isn't a bitfield and we were doing bitfields before,
        // finish this current byte off.
        if (state->bitfield_offset != 0)
        {
            state->bitfield_offset = 0;
            state->size++;
        }

        // Check if we need to align up and act accordingly
        if (NEED_ROUND_UP(state->size, type_align))
        {
            state->size = ROUND_UP(state->size, type_align);
            state->padding = true;
        }

        layout_add_entry(state, member, state->size, 0);

        // printf("offset is: %zu\n", state->size);

        // Increase the size of the struct by the type as long as we aren't a
        // flexible array type
        if (!flex_array)
        {
            state->size += type_size;
        }
    }
}

static void finish_compound_layout(LayoutCalculator* state)
{
    // First do our check if the bitfield offset it 0
    if (state->bitfield_offset != 0)
    {
        state->bitfield_offset = 0;
        state->size++;
        state->padding = true;
    }

    // Finally, check if we need to round up at all and do so if needed. But we
    // first have to handle the case of having an empty struct / union
    if (state->size == 0 && state->alignment == 0)
    {
        // Leave size as 0 :)
        state->alignment = 1;
    }
    else if (state->size == 0)
    {
        // Also round up if we don't have 0 alignment but 0 size. Note that this
        // is needed since the below doesn't catch this if size is 0
        state->size = state->alignment;
        state->padding = true;
    }
    else if (NEED_ROUND_UP(state->size, state->alignment))
    {
        state->size = ROUND_UP(state->size, state->alignment);
        state->padding = true;
    }

    // Now, finish setting the size and alignment of our struct type.
    QualifiedType type = declaration_get_type(state->decalration);
    QualifiedType real_type = qualified_type_get_canonical(&type);

    Type* raw_type = qualified_type_get_raw(&real_type);
    
    type_struct_set_size(raw_type, state->size, state->alignment);

    CompoundLayout* layout = create_layout(state->allocator, state->size,
            state->alignment, state->first);
    type_struct_set_layout(raw_type, layout);

    // printf("size: %zu, alignment: %zu\n", state->size, state->alignment);
    // return (CompoundLayout) {.entries = state->first, .size = state->size, .alignment = state->alignment};
}

void calculate_compound_layout(AstAllocator* allocator, DiagnosticManager* dm,
        Declaration* declaration)
{
    assert(declaration_is(declaration, DECLARATION_STRUCT)
        || declaration_is(declaration, DECLARATION_UNION));

    // First create the layout calculator we 
    LayoutCalculator layout_state = (LayoutCalculator)
    {
        .allocator = allocator,
        .dm = dm,
        .decalration = declaration,
        .size = 0,
        .alignment = 0,
        .is_union = declaration_is(declaration, DECLARATION_UNION),
        .padding = false,
        .first = NULL,
        .recent = NULL
    };

    DeclarationList members = declaration_struct_get_members(declaration);
    DeclarationListEntry* member_entry = declaration_list_iter(&members);
    while (member_entry != NULL)
    {
        // Get the member from the declaration.
        Declaration* member = declaration_list_entry_get(member_entry);

        // Go and do this iteration of the calculating
        calculate_field_layout(&layout_state, member);
        
        // Finally get the next member of the struct and continue
        member_entry = declaration_list_next(member_entry);
    }

    finish_compound_layout(&layout_state);
}
