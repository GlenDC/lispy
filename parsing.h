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
  LVAL_BOOLEAN,
  LVAL_ERROR,
  LVAL_SYMBOL,
  LVAL_SEXPR,
  LVAL_QEXPR,
  LVAL_FUNCTION,
  LVAL_STRING,
};

typedef struct lval_t {
  enum lval_type type;
  union {
    char* error;
    char* symbol;
    char* string;
    long number;
    double decimal;
    char boolean;
    struct sexpr {
      int count;
      struct lval_t** cell;
    } sexpr;
    struct function {
      lbuiltin builtin;
      struct lenv_t* env;
      struct lval_t* formals;
      struct lval_t* body;
    } function;
  } value;
} lval_t;

typedef struct lenv_val_t {
  char* symbol;
  struct lval_t* value;
} lenv_val_t;

typedef struct lenv_t {
  lenv_t* parent;
  int count;
  struct lenv_val_t* values;
} lenv_t;

typedef struct {
  mpc_parser_t *number, *decimal, *boolean,
    *symbol, *expr, *sexpr, *qexpr,
    *lispy, *string, *comment;
  struct lenv_t* env;
} LispyParser;

LispyParser* new_parser(int argc, char* argv[]);
void destroy_parser(LispyParser** parser);

int eval_and_print(LispyParser* parser, char* input);

#endif // __LISPY_PARSING__
