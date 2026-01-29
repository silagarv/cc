#include "lang.h"

#include <assert.h>

bool lang_standard_c89(LangStandard standard)
{
    assert(standard != LANG_STANDARD_DEFAULT);

    return standard >= LANG_STANDARD_C89;
}

bool lang_standard_c99(LangStandard standard)
{
    assert(standard != LANG_STANDARD_DEFAULT);

    return standard >= LANG_STANDARD_C99;
}

bool lang_standard_c11(LangStandard standard)
{
    assert(standard != LANG_STANDARD_DEFAULT);

    return standard >= LANG_STANDARD_C11;
}

bool lang_standard_c23(LangStandard standard)
{
    assert(standard != LANG_STANDARD_DEFAULT);

    return standard >= LANG_STANDARD_C23;
}
