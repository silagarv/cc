#include "statement.h"

#include <stdlib.h>

#include "util/xmalloc.h"

static Statement* statement_create_base(StatementType type)
{
    Statement* stmt = xmalloc(sizeof(Statement));
    stmt->base.type = type;

    return stmt;
}

Statement* statement_create_error(void)
{
    return statement_create_base(STATEMENT_ERROR);
}

Statement* statement_create_label(Location identifier_location, 
        Location colon_location, DeclarationLabel* label_decl, 
        Statement* statement)
{
    Statement* stmt = statement_create_base(STATEMENT_LABEL);

    stmt->label_stmt.identifier_location = identifier_location;
    stmt->label_stmt.colon_location = colon_location;
    stmt->label_stmt.label = label_decl;
    stmt->label_stmt.statement = statement;

    return stmt;
}

// TODO: other statement types

Statement* statement_create_goto(Location goto_location, Location semi_location,
        Declaration* label)
{
    Statement* stmt = statement_create_base(STATEMENT_GOTO);
    stmt->goto_stmt.goto_location = goto_location;
    stmt->goto_stmt.semi_location = semi_location;
    stmt->goto_stmt.label = label;

    return stmt;
}

Statement* statement_create_contine(Location continue_location, 
        Location semi_location, Statement* target_location)
{
    Statement* stmt = statement_create_base(STATEMENT_CONTINUE);
    stmt->continue_stmt.continue_location = continue_location;
    stmt->continue_stmt.semi_location = semi_location;
    stmt->continue_stmt.target = target_location;

    return stmt;
}

Statement* statement_create_break(Location break_location,
        Location semi_location, Statement* target_location)
{
    Statement* stmt = statement_create_base(STATEMENT_BREAK);
    stmt->break_stmt.break_location = break_location;
    stmt->break_stmt.semi_location = semi_location;
    stmt->break_stmt.target = target_location;

    return stmt;
}

Statement* statement_create_return(Location return_location,
        Location semi_location, Expression* expression)
{
    Statement* stmt = statement_create_base(STATEMENT_RETURN);
    stmt->return_stmt.return_location = return_location;
    stmt->return_stmt.location_semi = semi_location;
    stmt->return_stmt.expression_opt = expression;

    return stmt;
}

Statement* statement_create_empty(Location semi_location)
{
    Statement* stmt = statement_create_base(STATEMENT_EMPTY);
    stmt->empty_stmt.semi_location = semi_location;

    return stmt;
}
