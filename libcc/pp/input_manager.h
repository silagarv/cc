#ifndef INPUT_MANAGER_H
#define INPUT_MANAGER_H

#include <stdbool.h>

typedef struct SearchpathEntry {
    char* path;
    bool is_system;
    struct SearchpathEntry* next;
} SearchpathEntry;

typedef struct Searchpath {
    SearchpathEntry* start;
    SearchpathEntry* curr; // For quick appending
} Searchpath;

typedef struct InputManager {
    Searchpath quote_paths;
    Searchpath bracket_paths;
    Searchpath system_paths;
    Searchpath after_paths;
} InputManager;

InputManager* input_manager_new(void);
void input_manager_delete(InputManager* manager);

#endif /* INPUT_MANAGER_H */
