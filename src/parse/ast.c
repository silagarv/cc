#include "ast.h"

AstContext ast_context_push_for(AstContext* context, Statement* for_stmt)
{
    AstContext old_ctx = *context;

    context->current_breakable = for_stmt;
    context->current_iteration = for_stmt;

    return old_ctx;
}

AstContext ast_context_push_do_while(AstContext* context, Statement* do_stmt)
{
    AstContext old_ctx = *context;

    context->current_breakable = do_stmt;
    context->current_iteration = do_stmt;

    return old_ctx;
}

AstContext ast_context_push_while(AstContext* context, Statement* while_stmt)
{
    AstContext old_ctx = *context;

    context->current_breakable = while_stmt;
    context->current_iteration = while_stmt;

    return old_ctx;
}

AstContext ast_context_push_switch(AstContext* context, Statement* switch_stmt)
{
    AstContext old_ctx = *context;

    context->current_breakable = switch_stmt;
    context->current_switch = switch_stmt;

    return old_ctx;
}

void ast_context_pop(AstContext* context, AstContext old)
{
    *context = old;
}

Statement* ast_context_current_iterable(const AstContext* context)
{
    return context->current_iteration;
}

Statement* ast_context_current_breakable(const AstContext* context)
{
    return context->current_breakable;
}

Statement* ast_context_current_switch(const AstContext* context)
{
    return context->current_switch;
}

