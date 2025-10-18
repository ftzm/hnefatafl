#ifndef GREATEST_EXTENSIONS_H
#define GREATEST_EXTENSIONS_H

#include "../vendor/greatest/greatest.h"

#define ASSERT_GT_FMTm(MSG, EXP, GOT, FMT)                          \
    do {                                                            \
        greatest_info.assertions++;                                 \
        if (!((EXP) > (GOT))) {                                     \
            GREATEST_FPRINTF(GREATEST_STDOUT, "\nExpected: ");      \
            GREATEST_FPRINTF(GREATEST_STDOUT, FMT, EXP);            \
            GREATEST_FPRINTF(GREATEST_STDOUT, " > ");               \
            GREATEST_FPRINTF(GREATEST_STDOUT, FMT, GOT);            \
            GREATEST_FPRINTF(GREATEST_STDOUT, "\n     Got: ");      \
            GREATEST_FPRINTF(GREATEST_STDOUT, FMT, EXP);            \
            GREATEST_FPRINTF(GREATEST_STDOUT, " <= ");              \
            GREATEST_FPRINTF(GREATEST_STDOUT, FMT, GOT);            \
            GREATEST_FPRINTF(GREATEST_STDOUT, "\n");                \
            GREATEST_FAILm(MSG);                                    \
        }                                                           \
    } while (0)

#define ASSERT_LT_FMTm(MSG, EXP, GOT, FMT)                          \
    do {                                                            \
        greatest_info.assertions++;                                 \
        if (!((EXP) < (GOT))) {                                     \
            GREATEST_FPRINTF(GREATEST_STDOUT, "\nExpected: ");      \
            GREATEST_FPRINTF(GREATEST_STDOUT, FMT, EXP);            \
            GREATEST_FPRINTF(GREATEST_STDOUT, " < ");               \
            GREATEST_FPRINTF(GREATEST_STDOUT, FMT, GOT);            \
            GREATEST_FPRINTF(GREATEST_STDOUT, "\n     Got: ");      \
            GREATEST_FPRINTF(GREATEST_STDOUT, FMT, EXP);            \
            GREATEST_FPRINTF(GREATEST_STDOUT, " >= ");              \
            GREATEST_FPRINTF(GREATEST_STDOUT, FMT, GOT);            \
            GREATEST_FPRINTF(GREATEST_STDOUT, "\n");                \
            GREATEST_FAILm(MSG);                                    \
        }                                                           \
    } while (0)

#endif