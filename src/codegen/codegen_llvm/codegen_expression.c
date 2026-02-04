#include "codegen_expression.h"

#include <assert.h>

#include "util/panic.h"

#include "parse/declaration.h"
#include "parse/literal_parser.h"
#include "parse/type.h"
#include "parse/expression.h"

#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_llvm.h"
#include "codegen/codegen_llvm/codegen_util.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>
#include <stdint.h>

static LLVMValueRef llvm_codegen_integer_constant(CodegenContext* ctx,
        const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_INTEGER_CONSTANT));

    CodegenLLVM* llvm = ctx->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef bb = llvm->basic_block;

    // Get the expression type and thus the llvm type for the integer constant
    QualifiedType expr_type = expression_get_qualified_type(expression);
    LLVMTypeRef llvm_ty = llvm_get_type(ctx, &expr_type);

    // Then get the actual integer value itself and then generate the constant
    IntegerValue val = expression_integer_get_value(expression);
    unsigned long long n = integer_value_get_value(&val);

    // TODO: should correctly get if the value is sign extended here!
    LLVMPositionBuilderAtEnd(b, bb);
    return LLVMConstInt(llvm_ty, n, false);    
}

static LLVMValueRef llvm_codegen_floating_constant(CodegenContext* ctx,
        const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_FLOATING_CONSTANT));

    CodegenLLVM* llvm = ctx->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef bb = llvm->basic_block;

    // Get the expression type and thus the llvm type for the integer constant
    QualifiedType expr_type = expression_get_qualified_type(expression);
    LLVMTypeRef llvm_ty = llvm_get_type(ctx, &expr_type);

    // Then get the actual integer value itself and then generate the constant
    FloatingValue float_value = expression_float_get_value(expression);
    double n = (double) floating_value_get_value(&float_value);
    LLVMPositionBuilderAtEnd(b, bb);
    return LLVMConstReal(llvm_ty, n);    
}

static LLVMValueRef llvm_codegen_character_constant(CodegenContext* ctx,
        const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_CHARACTER_CONSTANT));

    CodegenLLVM* llvm = ctx->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef bb = llvm->basic_block;

    // Get the expression type and thus the llvm type for the integer constant
    QualifiedType expr_type = expression_get_qualified_type(expression);
    LLVMTypeRef llvm_ty = llvm_get_type(ctx, &expr_type);

    // Then get the actual integer value itself and then generate the constant
    CharValue char_value = expression_character_get_value(expression);
    uint64_t n = char_value_get_value(&char_value);

    // TODO: should correctly get if the value is sign extended here!
    LLVMPositionBuilderAtEnd(b, bb);
    return LLVMConstInt(llvm_ty, n, false);    
}

static LLVMValueRef llvm_codegen_enumeration_constant(CodegenContext* ctx,
        const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_ENUMERATION_CONSTANT));

    CodegenLLVM* llvm = ctx->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef bb = llvm->basic_block;

    // Get the expression type and thus the llvm type for the integer constant
    QualifiedType expr_type = expression_get_qualified_type(expression);
    LLVMTypeRef llvm_ty = llvm_get_type(ctx, &expr_type);

    // Then get the actual integer value itself and then generate the constant
    int value = expression_enum_constant_get_value(expression);
    // TODO: should correctly get if the value is sign extended here!
    LLVMPositionBuilderAtEnd(b, bb);
    return LLVMConstInt(llvm_ty, value, false);    
}

LLVMValueRef llvm_codegen_reference_expression(CodegenContext* ctx, 
        const Expression* e)
{
    assert(expression_is(e, EXPRESSION_REFERENCE));

    // TODO: this only works for variables right now so what about functions
    // and that kind of thing?

    CodegenLLVM* llvm = ctx->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef bb = llvm->basic_block;

    // First get the declaration from the reference expression and then get the
    // corrosponding alloca for it (we can only handle variables for now)
    Declaration* declaration = expression_reference_get_decl(e);
    LLVMValueRef value = llvm_codegen_get_declaration(ctx, declaration);
    assert(value != NULL);

    // Now we need to build a read of the value and return that. First position
    // the builder, then get the type we want and build the load for this
    // declaration.
    QualifiedType type = declaration_get_type(declaration);
    LLVMTypeRef llvm_ty = llvm_get_type(ctx, &type);

    LLVMPositionBuilderAtEnd(b, bb);
    return LLVMBuildLoad2(b, llvm_ty, value, "");
}

LLVMValueRef llvm_codegen_lvalue_cast(CodegenContext* c, const Expression* e)
{
    assert(expression_is(e, EXPRESSION_LVALUE_CAST));

    Expression* inner = expression_lvalue_cast_get_inner(e);
    return llvm_codegen_reference_expression(c, inner);
}

LLVMOpcode llvm_cast_determine_opcode(const QualifiedType* to,
        const QualifiedType* from)
{
    assert(!qualified_type_is_compatible_no_quals(to, from));

    // All of our options are above...
    // LLVMTrunc          = 30,
    // LLVMZExt           = 31,
    // LLVMSExt           = 32,
    // LLVMFPToUI         = 33,
    // LLVMFPToSI         = 34,
    // LLVMUIToFP         = 35,
    // LLVMSIToFP         = 36,
    // LLVMFPTrunc        = 37,
    // LLVMFPExt          = 38,
    // LLVMPtrToInt       = 39,
    // LLVMIntToPtr       = 40,
    // LLVMBitCast        = 41,
    // LLVMAddrSpaceCast  = 60,
    panic("casts are not fully implemented yet!");
    return LLVMZExt;
}

LLVMValueRef llvm_codegen_cast(CodegenContext* ctx, const Expression* e,
        ExpressionType kind)
{
    assert(expression_is(e, EXPRESSION_CAST)
            || expression_is(e, EXPRESSION_CAST_IMPLICIT));

    // Typical llvm setup
    CodegenLLVM* llvm = ctx->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef bb = llvm->basic_block;

    QualifiedType type = expression_get_qualified_type(e);
    LLVMTypeRef llvm_ty = llvm_get_type(ctx, &type);

    Expression* castee = NULL;
    if (kind == EXPRESSION_CAST)
    {
        castee = expression_cast_get_inner(e);
    }
    else
    {
        castee = expression_implicit_cast_get_inner(e);
    }

    LLVMValueRef llvm_castee = llvm_codegen_expression(ctx, castee);

    QualifiedType castee_type = expression_get_qualified_type(castee);
    LLVMOpcode cast_type = llvm_cast_determine_opcode(&type, &castee_type);

    LLVMPositionBuilderAtEnd(b, bb);
    return LLVMBuildCast(b, cast_type, llvm_castee, llvm_ty, "");
}

LLVMValueRef llvm_codegen_parentheses(CodegenContext* c, const Expression* e)
{
    assert(expression_is(e, EXPRESSION_PARENTHESISED));

    Expression* inner = expression_parenthesised_get_innermost(e);
    return llvm_codegen_expression(c, inner);
}

LLVMIntPredicate expression_kind_to_int_predicate(ExpressionType expr_type,
        const QualifiedType* type)
{
    assert(qualified_type_is_integer(type) && "Should be an int type!");

    bool is_signed = qualified_type_is_signed(type);

    switch (expr_type)
    {
        case EXPRESSION_BINARY_LESS_THAN:
            return is_signed ? LLVMIntSLT : LLVMIntULT;

        case EXPRESSION_BINARY_GREATER_THAN:
            return is_signed ? LLVMIntSGT : LLVMIntUGT;

        case EXPRESSION_BINARY_LESS_THAN_EQUAL:
            return is_signed ? LLVMIntSLE : LLVMIntULE;

        case EXPRESSION_BINARY_GREATER_THAN_EQUAL:
            return is_signed ? LLVMIntSGE : LLVMIntUGE;

        case EXPRESSION_BINARY_EQUAL:
            return LLVMIntEQ;

        case EXPRESSION_BINARY_NOT_EQUAL:
            return LLVMIntNE;

        default:
            panic("called with a bad expression type!");
            return LLVMIntEQ;
    }
}

LLVMRealPredicate expression_kind_to_real_predicate(ExpressionType expr_type)
{
    panic("unimplemented function");
    return 0;
}

LLVMValueRef llvm_codegen_comparison(CodegenContext* ctx, const Expression* e,
        ExpressionType kind)
{
    assert(expression_is(e, EXPRESSION_BINARY_LESS_THAN)
            || expression_is(e, EXPRESSION_BINARY_GREATER_THAN)
            || expression_is(e, EXPRESSION_BINARY_LESS_THAN_EQUAL)
            || expression_is(e, EXPRESSION_BINARY_GREATER_THAN_EQUAL)
            || expression_is(e, EXPRESSION_BINARY_EQUAL)
            || expression_is(e, EXPRESSION_BINARY_NOT_EQUAL));

    // Get all of our llvm codegen stuff
    CodegenLLVM* llvm = ctx->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef bb = llvm->basic_block;

    Expression* lhs = expression_binary_get_lhs(e);
    LLVMValueRef llvm_lhs = llvm_codegen_expression(ctx, lhs);

    Expression* rhs = expression_binary_get_rhs(e);
    LLVMValueRef llvm_rhs = llvm_codegen_expression(ctx, rhs);

    // Also get the int predicate type
    QualifiedType lhs_type = expression_get_qualified_type(lhs);
    QualifiedType rhs_type = expression_get_qualified_type(rhs);
    assert(qualified_type_is_compatible_no_quals(&lhs_type, &rhs_type));

    LLVMPositionBuilderAtEnd(b, bb);
    if (qualified_type_is_pointer(&lhs_type))
    {
        // TODO: This
        return NULL;
    }
    if (qualified_type_is_integer(&lhs_type))
    {
        LLVMIntPredicate cmp_type = expression_kind_to_int_predicate(kind,
                &lhs_type);
        return LLVMBuildICmp(b, cmp_type, llvm_lhs, llvm_rhs, "");
    }
    else
    {   
        // Build a comparison between real numbers
        LLVMRealPredicate cmp_type = expression_kind_to_real_predicate(kind);
        return LLVMBuildFCmp(b, cmp_type, llvm_lhs, llvm_rhs, "");
    }   
}

LLVMValueRef llvm_codegen_expression(CodegenContext* c, const Expression* e)
{
    assert(e != NULL);

    ExpressionType kind = expression_get_kind(e);
    switch (kind)
    {
        case EXPRESSION_ARRAY_DECAY:
            panic("unimplemented arrays!");
            return NULL;

        case EXPRESSION_INTEGER_CONSTANT:
            return llvm_codegen_integer_constant(c, e);

        case EXPRESSION_FLOATING_CONSTANT:
            return llvm_codegen_floating_constant(c, e);

        case EXPRESSION_CHARACTER_CONSTANT:
            return llvm_codegen_character_constant(c, e);

        case EXPRESSION_ENUMERATION_CONSTANT:
            return llvm_codegen_enumeration_constant(c, e);

        case EXPRESSION_STRING_LITERAL:
            panic("unimplemented!");
            return NULL;

        case EXPRESSION_REFERENCE:
            // TODO: this ideally shouldn't be naked but is it a true error???
            return llvm_codegen_reference_expression(c, e);

        case EXPRESSION_LVALUE_CAST:
            return llvm_codegen_lvalue_cast(c, e);

        case EXPRESSION_CAST:
        case EXPRESSION_CAST_IMPLICIT:
            return llvm_codegen_cast(c, e, kind);

            

            EXPRESSION_ARRAY_ACCESS,
            EXPRESSION_FUNCTION_CALL,
            EXPRESSION_MEMBER_ACCESS,
            EXPRESSION_MEMBER_POINTER_ACCESS,
            EXPRESSION_COMPOUND_LITERAL,
            EXPRESSION_SIZEOF_TYPE,
            EXPRESSION_SIZEOF_EXPRESSION,

            // Unary operations
            EXPRESSION_UNARY_ADDRESS,
            EXPRESSION_UNARY_DEREFERENCE,
            EXPRESSION_UNARY_PLUS,
            EXPRESSION_UNARY_MINUS,
            EXPRESSION_UNARY_BIT_NOT,
            EXPRESSION_UNARY_NOT,
            EXPRESSION_UNARY_PRE_INCREMENT,
            EXPRESSION_UNARY_PRE_DECREMENT,
            EXPRESSION_UNARY_POST_INCREMENT,
            EXPRESSION_UNARY_POST_DECREMENT;

            // arithmeric
            EXPRESSION_BINARY_TIMES,
            EXPRESSION_BINARY_DIVIDE,
            EXPRESSION_BINARY_MODULO,
            EXPRESSION_BINARY_ADD,
            EXPRESSION_BINARY_SUBTRACT;

            // Shifts
            EXPRESSION_BINARY_SHIFT_LEFT,
            EXPRESSION_BINARY_SHIFT_RIGHT;

            // Comparisons
            case EXPRESSION_BINARY_LESS_THAN:
            case EXPRESSION_BINARY_GREATER_THAN:
            case EXPRESSION_BINARY_LESS_THAN_EQUAL:
            case EXPRESSION_BINARY_GREATER_THAN_EQUAL:
            case EXPRESSION_BINARY_EQUAL:
            case EXPRESSION_BINARY_NOT_EQUAL:
                return llvm_codegen_comparison(c, e, kind);

            // Bitwise operations
            EXPRESSION_BINARY_AND,
            EXPRESSION_BINARY_XOR,
            EXPRESSION_BINARY_OR;

            // Logical operations (might require special handling)
            EXPRESSION_BINARY_LOGICAL_AND,
            EXPRESSION_BINARY_LOGICAL_OR;

            // Assign here!
            EXPRESSION_BINARY_ASSIGN,
            EXPRESSION_BINARY_TIMES_ASSIGN,
            EXPRESSION_BINARY_DIVIDE_ASSIGN,
            EXPRESSION_BINARY_MODULO_ASSIGN,
            EXPRESSION_BINARY_ADD_ASSIGN,
            EXPRESSION_BINARY_SUBTRACT_ASSIGN,
            EXPRESSION_BINARY_SHIFT_LEFT_ASSIGN,
            EXPRESSION_BINARY_SHIFT_RIGHT_ASSIGN,
            EXPRESSION_BINARY_AND_ASSIGN,
            EXPRESSION_BINARY_XOR_ASSIGN,
            EXPRESSION_BINARY_OR_ASSIGN;

            // Comma expression
            EXPRESSION_COMMA;

            // Conditional expression
            EXPRESSION_CONDITIONAL;

        case EXPRESSION_PARENTHESISED:
            return llvm_codegen_parentheses(c, e);

        case EXPRESSION_ERROR:
            panic("attmpeting to codegen an error expression");
            return NULL;

        default:
            panic("bad expression or don't know how to gen for it");
            return NULL;
    }
}

LLVMValueRef llvm_codegen_condition(CodegenContext* ctx, const Expression* e)
{
    // If we are already a comparison type expression generate IR for that
    ExpressionType kind = expression_get_kind(e);
    switch (kind)
    {
        case EXPRESSION_BINARY_LESS_THAN:
        case EXPRESSION_BINARY_GREATER_THAN:
        case EXPRESSION_BINARY_LESS_THAN_EQUAL:
        case EXPRESSION_BINARY_GREATER_THAN_EQUAL:
        case EXPRESSION_BINARY_EQUAL:
        case EXPRESSION_BINARY_NOT_EQUAL:
            return llvm_codegen_comparison(ctx, e, kind);

        default:
            break;
    }

    // Special case where the expression is not a comparison already we convert
    // it into on in this particular instance. This is done simply to make
    // the generated IR a bit more legible
    CodegenLLVM* llvm = ctx->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef bb = llvm->basic_block;

    // Make sure to generate the code for the expression
    LLVMValueRef llvm_expr = llvm_codegen_expression(ctx, e);

    // Then get the type of the expression we generated and compare it with 0
    LLVMTypeRef llvm_type = LLVMTypeOf(llvm_expr);
    LLVMValueRef llvm_zero = LLVMConstInt(llvm_type, 0, false);

    // Finally build the comparison so that we are able to properly compare the
    // result.
    LLVMPositionBuilderAtEnd(b, bb);
    return LLVMBuildICmp(b, LLVMIntNE, llvm_expr, llvm_zero, "");
}
