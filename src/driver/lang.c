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

bool lang_opts_c89(const LangOptions* opts)
{
    return lang_standard_c89(opts->standard);
}

bool lang_opts_c99(const LangOptions* opts)
{
    return lang_standard_c99(opts->standard);
}

bool lang_opts_c11(const LangOptions* opts)
{
    return lang_standard_c11(opts->standard);
}

bool lang_opts_c23(const LangOptions* opts)
{
    return lang_standard_c23(opts->standard);
}

bool lang_opts_strict(const LangOptions* opts)
{
    return opts->strict;
}

bool lang_opts_gnu(const LangOptions* opts)
{
    return opts->gnu;
}

LangOptions lang_opts(LangStandard std, bool strict, bool gnu)
{
    return (LangOptions) { std, strict, gnu };
}
