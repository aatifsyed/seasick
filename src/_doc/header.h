#include <stdbool.h>

typedef struct clothes
{
    bool hat;
    bool scarf;
} clothes;

typedef struct yak_shaver
{
    unsigned int id;
    char name[64];     // nul-terminated in array
    const char *owner; // borrowed from rt, not nul
    char *description; // owned, maybe nul
    char **children;   // nul-terminated array of runtime-owned strings
    clothes *clothes;  // owned, not nul
    struct
    {
        struct yak_shaver *next;
        struct yak_shaver *prev;
    } list; // intrusive ll
} yak_shaver;
