#include "lang.h"

bool lang_standard_c89(LangStandard standard)
{
    return standard >= LANG_STANDARD_C89;
}

bool lang_standard_c99(LangStandard standard)
{
    return standard >= LANG_STANDARD_C99;
}

bool lang_standard_c11(LangStandard standard)
{
    return standard >= LANG_STANDARD_C11;
}

bool lang_standard_c23(LangStandard standard)
{
    return standard >= LANG_STANDARD_C23;
}
