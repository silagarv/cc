#include "parser_new.h"

DeclarationGroup parse_declaration(Parser* parser, ParserDeclaratorContext ctx,
        Location* semi)
{
    return decl_group_from_empty();
}

QualifiedType parse_type_name(Parser *parser)
{
    return (QualifiedType) {0};
}

