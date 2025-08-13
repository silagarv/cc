#include "source_manager.h"

#include <string.h>

#include "util/panic.h"
#include "util/hash.h"
#include "util/hash_map.h"

#include "files/filepath.h"
#include "files/file_manager.h"

SourceManager source_manager(void)
{
    SourceManager sm = (SourceManager)
    {
        //fm = hash_map_create(filemap_hash_function, 
         //       filemap_key_compare_function, filemap_free_function),
        .highest_location = 0
    };

    return sm;
}

void source_manager_delete(SourceManager* sm)
{

}
