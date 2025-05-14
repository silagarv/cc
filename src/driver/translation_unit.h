#ifndef TRANSLATION_UNIT_H
#define TRANSLATION_UNIT_H

#include "parse/location.h"
#include "parse/input.h"

typedef struct TranslationUnit {
    InputManager input_manager;
    LocationMap location_manager;

    
} TranslationUnit;

#endif /* TRANSLATION_UNIT_H */
