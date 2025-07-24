#ifndef TARGET_H
#define TARGET_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef enum TargetByteOrder {
    TARGET_BYTE_ORDER_LITTLE_ENDIAN,
    TARGET_BYTE_ORDER_BIG_ENDIAN
} TargetByteOrder;

// A strucut to hold information about the target we would like to build for
// all of this information it based on the systemV abi and may change if I
// decide to add support for other stuff in the future
typedef struct Target {
    // Below are the sizes of the following types
    size_t sizeof_bool;
    size_t sizeof_char;
    size_t sizeof_short;
    size_t sizeof_int;
    size_t sizeof_long;
    size_t sizeof_long_long;
    size_t sizeof_float;
    size_t sizeof_double;
    size_t sizeof_long_double;
    size_t sizeof_pointer;
    size_t sizeof_function_pointer;

    // Below is the alignment of the following types
    size_t alignof_bool;
    size_t alignof_char;
    size_t alignof_short;
    size_t alignof_int;
    size_t alignof_long;
    size_t alignof_long_long;
    size_t alignof_float;
    size_t alignof_double;
    size_t alignof_long_double;
    size_t alignof_pointer;
    size_t alignof_function_pointer;

    // Is the base character type signed
    bool is_char_signed;

    // Get the byte order
    TargetByteOrder order;
} Target;

// Get the max value based on the size (in bytes)
uint64_t get_max_value(size_t bytes);

// Create a compiler target for x86-64-linux
Target target_create_x86_64_linux(void);

#endif /* TARGET_H */
