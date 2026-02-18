#include "driver/warning.h"
#ifndef WARNING
#error "The WARNING macro should be defined when including this file."
#endif /* WARNING */

// PP #warning

WARNING(Wabi, DIAG_STATE_ON, "abi") // GCC compat
WARNING(Waggregate_return, DIAG_STATE_ON, "aggregate-return") // GCC compat
WARNING(Wbuiltin_macro_redefined, DIAG_STATE_ON, "builtin-macro-redefined")
WARNING(Wc23_extensions, DIAG_STATE_ON, "c23-extensions")
WARNING(Wc99_compat, DIAG_STATE_ON, "c99-compat")
WARNING(Wc99_extensions, DIAG_STATE_ON, "c99-extensions")
WARNING(Wcast_qual, DIAG_STATE_ON, "cast-qual")
WARNING(Wchar_align, DIAG_STATE_ON, "char-align") // GCC compat
WARNING(Wchar_subscript, DIAG_STATE_ON, "char-subscript")
WARNING(Wcomma, DIAG_STATE_ON, "comma")
WARNING(Wcomment, DIAG_STATE_ON, "comment")
WARNING(Wdangling_else, DIAG_STATE_ON, "dangling-else")
WARNING(Wdate_time, DIAG_STATE_ON, "date-time")
WARNING(Wdeclaration_after_statement, DIAG_STATE_ON,
        "declaration-after-statement")
WARNING(Wdeprecated, DIAG_STATE_ON, "deprecated")
WARNING(Wdollar_in_identifier, DIAG_STATE_ON, "dollar-in-identifier")
WARNING(Wdouble_promotion, DIAG_STATE_ON, "double-promotion")
WARNING(Wduplicate_decl_spec, DIAG_STATE_ON, "duplicate-decl-specifier")
WARNING(Wembedded_directive, DIAG_STATE_ON, "embedded-directive")
WARNING(Wempty_body, DIAG_STATE_ON, "empty-body")
WARNING(Wempty_translation_unit, DIAG_STATE_ON, "empty-translation-unit")
WARNING(Wenum_too_large, DIAG_STATE_ON, "enum_too_large")
WARNING(Wexcess_initializers, DIAG_STATE_ON, "excess-initializers")
WARNING(Wexpansion_to_defined, DIAG_STATE_ON, "expansion-to-defined")
WARNING(Wexperimental, DIAG_STATE_ON, "experimental")
WARNING(Wextern_initializer, DIAG_STATE_ON, "extern-initializer")
WARNING(Wextra_semi, DIAG_STATE_ON, "extra-semi")
WARNING(Wextra_tokens, DIAG_STATE_ON, "extra-tokens")
WARNING(Wflexible_array_exts, DIAG_STATE_ON, "flexible-array-extensions")
WARNING(Wgnu_case_range, DIAG_STATE_ON, "gnu-case-range")
WARNING(Wgnu_empty_struct, DIAG_STATE_ON, "gnu-empty-struct")
WARNING(Wgnu_include_next, DIAG_STATE_ON, "gnu-include-next")
WARNING(Whash_warning, DIAG_STATE_ON, "#warning")
WARNING(Wignored_attributes, DIAG_STATE_ON, "ignored-attributes")
WARNING(Wignored_pragmas, DIAG_STATE_ON, "ignored-pragmas")
WARNING(Wignored_qualifiers, DIAG_STATE_ON, "ignored-qualifiers")
WARNING(Wimplicit_fallthrough, DIAG_STATE_ON, "implicit-fallthrough")
WARNING(Wimplicit_int, DIAG_STATE_ON | DIAG_STATE_ERROR, "implicit-int")
WARNING(Wimplicit_function_declaration, DIAG_STATE_ON | DIAG_STATE_ERROR,
        "implicit-function-declaration")
WARNING(Winclude_next_outside_header, DIAG_STATE_ON,
        "include-next-outside-header")
WARNING(Wincompatible_function_pointer_types, DIAG_STATE_ON | DIAG_STATE_ERROR,
        "incompatible-function-pointer-types")
WARNING(Wincompatible_pointer_types, DIAG_STATE_ON,
        "incompatible-pointer-types")
WARNING(Wincompatible_pointer_types_discards_qualifiers, DIAG_STATE_ON,
        "incompatible-pointer-types-discards-qualifiers")
WARNING(Wint_conversion, DIAG_STATE_ON | DIAG_STATE_ERROR, "int-conversion")
WARNING(Winvalid_pp_token, DIAG_STATE_ON, "invalid-pp-token")
WARNING(Winvalid_token_paste, DIAG_STATE_ON | DIAG_STATE_ERROR,
        "invalid-token-paste")
WARNING(Wlanguage_extension_token, DIAG_STATE_ON,"language-extension-token")
WARNING(Wlong_long, DIAG_STATE_ON, "long-long")
WARNING(Wmacro_redefined, DIAG_STATE_ON, "macro-redefined")
WARNING(Wmain, DIAG_STATE_ON, "main")
WARNING(Wmany_braces, DIAG_STATE_ON, "many-braces-around-scalar-init")
WARNING(Wmissing_decls, DIAG_STATE_ON, "missing-declarations")
WARNING(Wmultichar, DIAG_STATE_ON, "multichar")
WARNING(Wnewline_eof, DIAG_STATE_ON, "newline-eof")
WARNING(Wother, DIAG_STATE_ON, "other")
WARNING(Woverflow, DIAG_STATE_ON, "overflow")
WARNING(Wshadow, DIAG_STATE_ON, "shadow")
WARNING(Wstatic_in_inline, DIAG_STATE_ON, "static-in-inline")
WARNING(Wstatic_local_in_inline, DIAG_STATE_ON, "static-local-in-inline")
WARNING(Wswitch, DIAG_STATE_ON, "switch")
WARNING(Wtentative, DIAG_STATE_ON, "tentative")
WARNING(Wtrigraphs, DIAG_STATE_ON, "trigraphs")
WARNING(Wtypedef_redefinition, DIAG_STATE_ON, "typedef-redefinition")
WARNING(Wundef, DIAG_STATE_ON, "undef")
WARNING(Wunicode, DIAG_STATE_ON, "unicode")
WARNING(Wunknown_warning_opt, DIAG_STATE_ON, "unknown-warning-option")
WARNING(Wunknown_escape_sequence, DIAG_STATE_ON, "unknown-escape-sequence")
WARNING(Wvisibility, DIAG_STATE_ON, "visibility")
WARNING(Wvla_extension, DIAG_STATE_ON, "vla-extension")
WARNING(Wzero_as_null_ptr, DIAG_STATE_ON, "zero-as-null-pointer-constant")
WARNING(Wzero_length_array, DIAG_STATE_ON | DIAG_STATE_ERROR,
        "zero-length-array")

#undef WARNING
