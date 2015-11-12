#ifndef __LISPY_PARSING__
#define __LISPY_PARSING__

#include "mpc.h"

typedef struct {
  mpc_parser_t *number, *decimal, *operator, *expression, *lispy;
} LispyParser;

LispyParser* new_parser();
void destroy_parser(LispyParser** parser);

int eval_and_print(LispyParser* parser, char* input);

#endif // __LISPY_PARSING__
