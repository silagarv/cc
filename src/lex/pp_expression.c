#include "pp_expression.h"

#include "driver/diagnostic.h"

#include "lex/token.h"
#include "lex/preprocessor.h"

#include "parse/literal_parser.h"

typedef enum Precedance {
    PRECEDENCE_CONDITIONAL
} Precedance;

int64_t pp_value_get_value(const PPValue* value)
{
    return value->value;
}

bool pp_value_get_success(const PPValue* value)
{
    return value->success;
}

bool preprocessor_parse_subexpression(Preprocessor* pp, Token* token,
        PPValue* value, Precedance prec)
{
    value->success = false;
    return false;
}

bool preprocessor_parse_expression(Preprocessor* pp, Token* token,
        PPValue* value)
{



    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "unimplemented directive handling...");

    value->success = false;
    return false;
}
