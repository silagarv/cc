#include "lexer.h"

#include <stddef.h>
#include <assert.h>

#include "parse/location.h"
#include "parse/line.h"
#include "parse/input.h"
#include "parse/token.h"

int input_peek_char(Input* input, size_t peek_size)
{
    char* current_line = input->current_line->line_buffer;

    return current_line[input->curr_line_idx + peek_size];
}

int input_current_char(Input* input)
{
    return input_peek_char(input, 0);
}

void input_consume_char(Input* input)
{
    if (input->current_line->line_length == input->curr_line_idx) 
    {
        assert(input_current_char(input) == '\n');
        // get next line handles updating to the correct position
        input_get_next_line(input);
        return;
    }

    input->curr_line_idx++;

    input->location.col_no++;
}



