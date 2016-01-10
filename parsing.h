#ifndef __LISPY_PARSING__
#define __LISPY_PARSING__

#include "mpc.h"

struct lenv_t;
struct lval_t;

typedef struct lenv_t lenv_t;
typedef struct lval_t lval_t;

typedef lval_t*(*lbuiltin)(lenv_t*, lval_t*);

enum lval_type {
  LVAL_NUMBER,
  LVAL_DECIMAL,
  LVAL_ERROR,
  LVAL_SYMBOL,
  LVAL_SEXPR,
  LVAL_QEXPR,
  LVAL_FUN,
};

typedef struct lval_t {
  enum lval_type type;
  union {
    char* error;
    char* symbol;
    long number;
    double decimal;
    struct sexpr {
      int count;
      struct lval_t** cell;
    } sexpr;
    lbuiltin fun;
  } value;
} lval_t;

typedef struct lenv_val_t {
  char* symbol;
  lval_t* value;
} lenv_val_t;

typedef struct lenv_t {
  int count;
  lenv_val_t* values;
} lenv_t;

typedef struct {
  mpc_parser_t *number, *decimal, *symbol, *expr, *sexpr, *qexpr, *lispy;
  lenv_t* env;
} LispyParser;

LispyParser* new_parser();
void destroy_parser(LispyParser** parser);

int eval_and_print(LispyParser* parser, char* input);

#endif // __LISPY_PARSING__
