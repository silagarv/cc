#ifndef INPUT_H
#define INPUT_H

#include <stddef.h>
#include <stdio.h>

#include "adt/arena.h"
#include "adt/vector.h"

#include "parse/location.h"
#include "parse/line.h"

// TODO: possible problem... what if sometimes the location is different???
// TODO: I.e. in one instance of file opening a #line is done but in another
// TODO: it is not. This might lead to confusion possibly
// TODO: maybe instead still keep the real / believed location of each line
// TODO: but could maybe have a map which assigns id's to location's that we get
// TODO: then since these are global and sequestial they are O(1) lookup

// Structure to hold an entry to a search path we want to go to
typedef struct SearchPathEntry {
    char* filepath;
    bool is_system;

    struct SearchPathEntry* next;
} SearchPathEntry;

// A list of search paths to go through 
typedef struct SearchPath {
    SearchPathEntry* start;
    SearchPathEntry** anchor;
} SearchPath;

// Struct to store the current state of the input being read
typedef struct InputReader {
    FILE* fp; // the file to read from

    // information for the buffer for reading characters from it
    char* buffer;
    size_t cap;

    size_t pos;
    size_t len;

    int c; // the previous char

    SourceLocation location; // the location within the file 
} InputReader;

typedef struct Input {
    char* filename; // the filename we were given for this input

    // Vector(Line)
    Vector lines; // the lines we have read from the input

    size_t curr_line_idx; // the index of the current line we have
    Line* current_line; // a pointer to the line in lines

    size_t line_pos; // the current char we are on

    SourceLocation location; // the current location we think we are

    // note that columns will always be the same we just need to adjust
    // the filename and the line number so the location has enough info with
    // the current line for us

    size_t depth; // include depth for runaway inclusion
    SearchPathEntry* entry; // the entry we found this input at
    struct Input* parent; // the parent input
} Input;

// A structure which represents all of the inputs that we have managed
typedef struct InputManager {
    // vector(Input*) inputs;
    Vector inputs;

    // This is for one main reason. We can change filename but we can't free the
    // ptr since what if a token uses that pointer. Then it is invalid, so we 
    // need to keep them around for the entire lifetime of the token. So since
    // input manager will be around for the same life time we can keep it

    // vector(char*) filenames;
    Vector filenames;

    // Our different include paths here
    SearchPath quote_paths;
    SearchPath bracket_paths;
    SearchPath system_paths;
    SearchPath after_paths;
} InputManager;

// Function to manage SearchPath's and their entries
SearchPathEntry* searchpath_entry_new(char* filepath, bool is_system);
void searchpath_entry_delete(SearchPathEntry* entry);

void searchpath_add_entry(SearchPath* search, SearchPathEntry* entry);
void searchpath_append(SearchPath* start, SearchPath* after);

// Functions to manage an input reader
InputReader* input_reader_from_file(FILE* fp, char* filename);
void input_reader_free(InputReader* reader);

Line input_reader_next_line(InputReader* reader);

// Functions to manage an input
Input* input_new(FILE* fp, char* filename, SearchPathEntry* entry);
void input_delete(Input* input);

void input_set_filename(Input* input, char* new_filename);
void input_set_line(Input* input, size_t new_line);

Line* input_get_next_line(Input* input);

Line* input_find_real_line(Input* input, size_t real_line);

SourceLocation input_get_location(Input* input);
SourceLocation input_get_real_location(Input* input);

// Functions to manage an input manager
InputManager* input_manager_new(void);
void input_manager_delete(InputManager* manager);

char* input_manager_allocate_filename_buffer(InputManager* manager, size_t len);
char* input_manager_allocate_filename(InputManager* manager, char* filename);
char* input_manager_allocate_filename_len(InputManager* manager, char* filename,
        size_t len);

void add_searchpath(InputManager* arena, SearchPath* path, char* filename, 
        bool sys);

void input_manager_add_quote_path(InputManager* manager, char* filepath);
void input_manager_add_bracket_path(InputManager* manager, char* filepath);
void input_manager_add_system_path(InputManager* manager, char* filepath);
void input_manager_add_after_path(InputManager* manager, char* filepath);

void input_manager_finish_setup(InputManager* manager);

void input_manager_print_include_paths(InputManager* manager);

// TODO: rething how I want this to work...

FILE* input_manager_get_file(InputManager* manager, char* filepath);
FILE* input_manager_find_file(InputManager* manager, char* filename, 
        SearchPathEntry* entry, char* current_file);

Input* input_manager_get_input(InputManager* manager, char* filename);

// TODO: maybe add function input_manager_get_input(...)

Input* input_manager_find_real_file(InputManager* manager, char* filename);
Line* input_manager_find_real_line(InputManager* manager, char* filename,
        size_t real_line);

#endif /* INPUT_H */
