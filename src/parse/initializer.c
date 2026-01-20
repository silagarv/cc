#include "initializer.h"

#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "files/location.h"
#include "parse/ast_allocator.h"
#include "parse/expression.h"

Designator* designator_create_base(AstAllocator* allocator, size_t size,
        DesignatorType type)
{
    Designator* desig = ast_allocator_alloc(allocator, size);
    desig->base.type = type;

    return desig;
}

Designator* designator_create_member(AstAllocator* allocator, Location dot_loc,
        Location identifier_loc, Identifier* identifier)
{
    Designator* desig = designator_create_base(allocator,
            sizeof(DesignatorMember), DESIGNATOR_MEMBER);
    desig->member.dot_loc = dot_loc;
    desig->member.identifier_loc = identifier_loc;
    desig->member.identifier = identifier;

    return desig;
}

Designator* designator_create_array(AstAllocator* allocator, Location lbracket,
        Expression* expression, Location rbractet)
{
    Designator* desig = designator_create_base(allocator,
            sizeof(DesignatorArray), DESIGNATOR_ARRAY);
    desig->array.l_bracket_loc = lbracket;
    desig->array.r_bracket_loc = rbractet;
    desig->array.constant_expr = expression;

    return desig;
}

Location designator_get_location(const Designator* desig)
{
    if (designator_is(desig, DESIGNATOR_ARRAY))
    {
        return desig->array.l_bracket_loc;
    }
    else if (designator_is(desig, DESIGNATOR_MEMBER))
    {
        return desig->member.dot_loc;
    }
    
    panic("bad designator type");
    return LOCATION_INVALID;
}
    
DesignatorList* designator_list_create(AstAllocator* allocator,
        Designator* designator)
{
    DesignatorList* list = ast_allocator_alloc(allocator,
            sizeof(DesignatorList));
    list->designator = designator;
    list->next = NULL;

    return list;
}

bool designator_is(const Designator* designsotr, DesignatorType type)
{
    return designsotr->base.type == type;
}

Designator* designator_list_get_member(const DesignatorList* list)
{
    return list->designator;
}

DesignatorList* designator_list_set_next(DesignatorList* list,
        DesignatorList* next)
{
    if (list != NULL)
    {
        list->next = next;
    }

    return next;
}

DesignatorList* designator_list_get_next(const DesignatorList* list)
{
    return list->next;
}

InitializerListMember* initializer_list_member_create(AstAllocator* allocator,
        DesignatorList* designator, Location equals_loc, Initializer* init)
{
    InitializerListMember* member = ast_allocator_alloc(allocator,
            sizeof(InitializerListMember));
    member->designation = designator;
    member->equals_loc = equals_loc;
    member->initializer = init;
    member->next = NULL;

    return member;
}

DesignatorList* initializer_list_member_get_designator(
        const InitializerListMember* member)
{
    return member->designation;
}

Location initializer_list_member_get_equals(const InitializerListMember* mem)
{
    return mem->equals_loc;
}

Initializer* initializer_list_member_get_initializer(
        const InitializerListMember* member)
{
    return member->initializer;
}

InitializerListMember* initializer_list_member_set_next(
        InitializerListMember* member, InitializerListMember* next)
{
    if (member != NULL)
    {
        member->next = next;
    }

    return next;
}

InitializerListMember* initializer_list_member_get_next(
        InitializerListMember* member)
{
    return member->next;
}

static Initializer* initializer_create_base(AstAllocator* allocator,
        size_t size, InitializerType type)
{
    Initializer* init = ast_allocator_alloc(allocator, size);
    init->base.type = type;

    return init;
}

Initializer* initializer_create_expression(AstAllocator* allocator,
        Expression* expression)
{
    Initializer* init = initializer_create_base(allocator,
            sizeof(InitializerExpression), INITIALIZER_EXPRESSION);
    init->expr.expr = expression;

    return init;
}

Initializer* initializer_create_list(AstAllocator* allocator, Location lcurly,
        InitializerListMember* first_member, Location rcurly)
{
    Initializer* init = initializer_create_base(allocator,
            sizeof(InitializerList), INITIALIZER_LIST);
    init->list.left_backet = lcurly;
    init->list.right_bracket = rcurly;
    init->list.head = first_member;

    return init;
}

bool initializer_is(const Initializer* init, InitializerType type)
{
    return init->base.type == type;
}

Expression* initializer_expression_get(const Initializer* init)
{
    assert(initializer_is(init, INITIALIZER_EXPRESSION));

    return init->expr.expr;
}

Location initializer_expression_location(const Initializer* init)
{
    assert(initializer_is(init, INITIALIZER_EXPRESSION));

    return expression_get_location(init->expr.expr);
}

InitializerListMember* initializer_list_member_get(const Initializer* init)
{
    assert(initializer_is(init, INITIALIZER_LIST));

    return init->list.head;
}

Location initializer_list_lcurly(const Initializer* init)
{
    assert(initializer_is(init, INITIALIZER_LIST));

    return init->list.left_backet;
}

Location initializer_list_rcurly(const Initializer* init)
{
    assert(initializer_is(init, INITIALIZER_LIST));

    return init->list.right_bracket;
}

bool initializer_list_has_designator(const Initializer* init)
{
    assert(initializer_is(init, INITIALIZER_LIST));

    InitializerListMember* member = initializer_list_member_get(init);
    for (; member; member = initializer_list_member_get_next(member))
    {
        if (initializer_list_member_get_designator(member) != NULL)
        {
            return true;
        }
    }

    return false;
}

bool initializer_is_empty(const Initializer* init)
{
    if (initializer_is(init, INITIALIZER_EXPRESSION))
    {
        return false;
    }

    InitializerListMember* first = initializer_list_member_get(init);

    return first == NULL;
}


