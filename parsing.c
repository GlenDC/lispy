
#include "parsing.h"

LispyParser* new_parser() {
  LispyParser* parser = malloc(sizeof(LispyParser));

  // Create some parsers
  parser->number = mpc_new("number");
  parser->decimal = mpc_new("decimal");
  parser->symbol = mpc_new("symbol");
  parser->expr = mpc_new("expr");
  parser->sexpr = mpc_new("sexpr");
  parser->qexpr = mpc_new("qexpr");
  parser->lispy = mpc_new("lispy");

  // Define them with the following language
  mpca_lang(MPCA_LANG_DEFAULT,
    "                                                         \
      decimal: /-?[0-9]+[.][0-9]+/;                           \
      number: /-?[0-9]+/;                                     \
      symbol: '+' | '-' | '*' | '/' | '%' | '^'		      \
            | \"list\" | \"head\" | \"tail\"		      \
            | \"join\" | \"eval\" | \"cons\"		      \
            | \"len\" | \"init\";			      \
      sexpr: '(' <expr>* ')';	                              \
      qexpr: '{' <expr>* '}';						\
      expr: <symbol> | <decimal> | <number> | <sexpr> | <qexpr>;	      \
      lispy: /^/ <expr>* /$/;                                 \
    ",
    parser->number,
    parser->decimal,
    parser->symbol,
    parser->sexpr,
    parser->expr,
    parser->qexpr,
    parser->lispy);

  return parser;
}

void destroy_parser(LispyParser** parser) {
  if(*parser) {
    LispyParser* _parser = *parser;
    // Undefine and Delete our parser
    mpc_cleanup(
      7,
      _parser->number,
      _parser->decimal,
      _parser->symbol,
      _parser->expr,
      _parser->sexpr,
      _parser->qexpr,
      _parser->lispy);

    free(_parser);
    parser = NULL;
  }
}

enum lval_type {
  LVAL_NUMBER,
  LVAL_DECIMAL,
  LVAL_ERROR,
  LVAL_SYMBOL,
  LVAL_SEXPR,
  LVAL_QEXPR,
};

typedef struct lval_t {
  enum lval_type type;
  union {
    char* error;
    char* symbol;
    long number;
    double decimal;
    struct {
      int count;
      struct lval_t** cell;
    } sexpr;
  } value;
} lval_t;

#define LVAL_ASSERT(_v_, _cond_, _msg_) \
  if(!(_cond_)) {			\
    lval_delete(_v_);				\
    return lval_error(_msg_);			\
  }
    
lval_t* lval_number(long number) {
  lval_t* val = malloc(sizeof(lval_t));
  val->type = LVAL_NUMBER;
  val->value.number = number;
  return val;
}

lval_t* lval_decimal(double decimal) {
  lval_t* val = malloc(sizeof(lval_t));
  val->type = LVAL_DECIMAL;
  val->value.decimal = decimal;
  return val;
}

lval_t* lval_error(char* e) {
  lval_t* val = malloc(sizeof(lval_t));
  val->type = LVAL_ERROR;
  val->value.error = malloc(strlen(e) + 1);
  strcpy(val->value.error, e);
  return val;
}

lval_t* lval_symbol(char* s) {
  lval_t* val = malloc(sizeof(lval_t));
  val->type = LVAL_SYMBOL;
  val->value.symbol = malloc(strlen(s) + 1);
  strcpy(val->value.symbol, s);
  return val;
}

lval_t* lval_sexpr(void) {
  lval_t* val = malloc(sizeof(lval_t));
  val->type = LVAL_SEXPR;
  val->value.sexpr.count = 0;
  val->value.sexpr.cell = NULL;
  return val;
}

lval_t* lval_qexpr(void) {
  lval_t* val = malloc(sizeof(lval_t));
  val->type = LVAL_QEXPR;
  val->value.sexpr.count = 0;
  val->value.sexpr.cell = NULL;
  return val;
}

void lval_delete(lval_t* v) {
  switch(v->type) {
    case LVAL_NUMBER: break;
    case LVAL_DECIMAL: break;

    case LVAL_ERROR:
      free(v->value.error);
      break;

    case LVAL_SYMBOL:
      free(v->value.symbol);
      break;

    case LVAL_SEXPR:
    case LVAL_QEXPR:
      for(int i = 0; i < v->value.sexpr.count; ++i) {
        lval_delete(v->value.sexpr.cell[i]);
      }

      free(v->value.sexpr.cell);
      break;
  }

  free(v);
}

lval_t* lval_read_number(mpc_ast_t* node) {
  errno = 0;
  long x = strtol(node->contents, NULL, 10);
  if(errno == ERANGE) {
    return lval_error("Tried to read a number, but it was out of range.");
  }

  return lval_number(x);
}

lval_t* lval_read_decimal(mpc_ast_t* node) {
  errno = 0;
  double x = strtod(node->contents, NULL);
  if(errno == ERANGE) {
    return lval_error("Tried to read a decimal, but it was out of range.");
  }

  return lval_decimal(x);
}

lval_t* lval_add(lval_t* v, lval_t* child) {
  v->value.sexpr.count++;
  v->value.sexpr.cell = realloc(v->value.sexpr.cell, sizeof(lval_t*) * v->value.sexpr.count);
  v->value.sexpr.cell[v->value.sexpr.count - 1] = child;
  return v;
}

lval_t* lval_read(mpc_ast_t* node) {
  if(strstr(node->tag, "number")) return lval_read_number(node);
  if(strstr(node->tag, "decimal")) return lval_read_decimal(node);

  if(strstr(node->tag, "symbol")) return lval_symbol(node->contents);
  
  lval_t* v = NULL;
  if(strcmp(node->tag, ">") == 0 || strstr(node->tag, "sexpr")) v = lval_sexpr();
  else if(strstr(node->tag, "qexpr")) v = lval_qexpr();
  else return NULL;

  for(int i = 0; i < node->children_num; ++i) {
    char* contents = node->children[i]->contents;
    if(strcmp(contents, "(") == 0
       || strcmp(contents, ")") == 0
       || strcmp(contents, "{") == 0
       || strcmp(contents, "}") == 0
       || strcmp(node->children[i]->tag, "regex") == 0) {
      continue;
    }

    v = lval_add(v, lval_read(node->children[i]));
  }

  return v;
}

void lval_print(lval_t* v);

void lval_expr_print(lval_t* v, char open, char close) {
  putchar(open);

  for(int i = 0; i < v->value.sexpr.count; ++i) {
    lval_print(v->value.sexpr.cell[i]);
    if(i != v->value.sexpr.count - 1) {
      putchar(' ');
    }
  }
  
  putchar(close);
}

void lval_print(lval_t* v) {
  switch(v->type) {
    case LVAL_NUMBER: printf("%li", v->value.number); break;
    case LVAL_DECIMAL: printf("%f", v->value.decimal); break;
    case LVAL_ERROR: printf("Error: %s", v->value.error); break;
    case LVAL_SYMBOL: printf("%s", v->value.symbol); break;
    case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break; 
    case LVAL_QEXPR: lval_expr_print(v, '{', '}'); break;
  }
}

void lval_println(lval_t* v) {
  lval_print(v);
  putchar('\n');
}

lval_t* lval_pop(lval_t* v, int i) {
  // Get The element at position i
  lval_t* x = v->value.sexpr.cell[i];

  // Shift Memory
  int offset = sizeof(lval_t*) * (v->value.sexpr.count - i - 1);
  memmove(&v->value.sexpr.cell[i], &v->value.sexpr.cell[i+1], offset);

  // Decrease count
  v->value.sexpr.count--;

  // Reallocate memory
  int sz = sizeof(lval_t*) * v->value.sexpr.count;
  v->value.sexpr.cell = realloc(v->value.sexpr.cell, sz);
  
  return x;
}

lval_t* lval_take(lval_t* v, int i) {
  lval_t* x = lval_pop(v, i);
  lval_delete(v);
  return x;
}

#define OP_ADD "+"
#define OP_SUB "-"
#define OP_MUL "*"
#define OP_DIV "/"
#define OP_POW "^"
#define OP_MOD "%"

#define APPLY_SUB(_x_, _y_, _cx_, _cy_) (_x_) -= (_y_)
#define APPLY_ADD(_x_, _y_, _cx_, _cy_) (_x_) += (_y_)
#define APPLY_MUL(_x_, _y_, _cx_, _cy_) (_x_) *= (_y_)
#define APPLY_POW(_x_, _y_, _cx_, _cy_) _x_ = pow((double)(_x_), (double)(_y_))
#define APPLY_DIV(_x_, _y_, _cx_, _cy_) \
    if((_y_) == 0) {\
      lval_delete(_cx_); lval_delete(_cy_); \
      (_cx_) = lval_error("Division By Zero!"); break; \
    } else { \
      (_x_) /= (_y_); \
    }\

#define ENSURE_DECIMAL(_x_) \
  if((_x_)->type != LVAL_DECIMAL) { \
    (_x_)->type = LVAL_DECIMAL; \
    (_x_)->value.decimal = (double)(_x_)->value.number; \
  }

#define APPLY_OP(_x_, _y_, _op_) \
    if((_x_)->type == LVAL_DECIMAL || (_y_)->type == LVAL_DECIMAL) { \
      ENSURE_DECIMAL(_x_);					     \
      ENSURE_DECIMAL(_y_);						\
      _op_((_x_)->value.decimal, (_y_)->value.decimal, _x_, _y_);	\
    } else { \
      _op_((_x_)->value.number, (_y_)->value.number, _x_, _y_);	\
    }

lval_t* builtin_op(lval_t* v, char* op) {
  // Ensure all elements are numbers
  for(int i = 0; i < v->value.sexpr.count; ++i) {
    // TODO: Optimise type to use masks for type groups (such as number types)
    if(v->value.sexpr.cell[i]->type != LVAL_NUMBER && v->value.sexpr.cell[i]->type != LVAL_DECIMAL) {
      lval_delete(v);
      return lval_error("Cannot operate on non-number!");
    }
  }

  lval_t* x = lval_pop(v, 0);

  // if no arguments and sub then perform unary negation
  if(strcmp(op, OP_SUB) == 0 && v->value.sexpr.count == 0) {
    if(x->type == LVAL_NUMBER)
      x->value.number = -x->value.number;
    else
      x->value.decimal = -x->value.decimal;
  }

  while(v->value.sexpr.count > 0) {
    lval_t* y = lval_pop(v, 0);

    if(strcmp(op, OP_SUB) == 0) { APPLY_OP(x, y, APPLY_SUB); }
    else if(strcmp(op, OP_ADD) == 0) { APPLY_OP(x, y, APPLY_ADD); }
    else if(strcmp(op, OP_MUL) == 0) { APPLY_OP(x, y, APPLY_MUL); }
    else if(strcmp(op, OP_DIV) == 0) { APPLY_OP(x, y, APPLY_DIV); }
    else if(strcmp(op, OP_POW) == 0) { APPLY_OP(x, y, APPLY_POW); }
    else if(strcmp(op, OP_MOD) == 0) {
      if(x->type == LVAL_NUMBER && y->type == LVAL_NUMBER) {
	x->value.number %= y->value.number;
      } else {
	lval_delete(x);
	lval_delete(y);
	x = lval_error("Can only apply the module operator to numbers!");
	break;
      }
    }

    lval_delete(y);
  }

  lval_delete(v);
  return x;
}

#define LVAL_ASSERT_ARGS_EQ(_v_, _n_, _fn_) \
  LVAL_ASSERT(_v_, (_v_)->value.sexpr.count == (_n_), \
	      "Invalid amount of arguments passed to '" _fn_ "'!")

lval_t* builtin_head(lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "head");
  
  lval_t* a = lval_take(v, 0);
  
  LVAL_ASSERT(a, a->type == LVAL_QEXPR,
	      "Passed an invalid type to 'head'!");
  LVAL_ASSERT(a, a->value.sexpr.count != 0,
	      "Passed an empty Q-Expression to 'head'!");

  // delete all elements except the head
  while(a->value.sexpr.count > 1)
    lval_delete(lval_pop(a, 1));

  return a;
}

lval_t* builtin_tail(lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "tail");
  
  lval_t* a = lval_take(v, 0);
  
  LVAL_ASSERT(a, a->type == LVAL_QEXPR,
	      "Passed an invalid type to 'tail'!");
  LVAL_ASSERT(a, a->value.sexpr.count != 0,
	      "Passed an empty Q-Expression to 'tail'!");

  // delete the head of the Q-Expression
  lval_delete(lval_pop(a, 0));

  return a;
}

lval_t* lval_eval(lval_t* v);

lval_t* builtin_list(lval_t* v) {
  LVAL_ASSERT(v, v->type == LVAL_SEXPR,
	      "Passed a type other than an S-Expression to 'list'!");

  v->type = LVAL_QEXPR;
  return v;
}

lval_t* builtin_eval(lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "eval");
  
  lval_t* a = lval_take(v, 0);
  LVAL_ASSERT(a, a->type == LVAL_QEXPR,
	      "Passed a type other than a Q-Expression to 'eval'!");

  a->type = LVAL_SEXPR;
  return lval_eval(a);
}

lval_t* lval_join(lval_t* a, lval_t* b) {
  while(b->value.sexpr.count > 0)
    lval_add(a, lval_pop(b, 0));

  lval_delete(b);
  return a;
}

lval_t* builtin_join(lval_t* v) {
  LVAL_ASSERT(v, v->value.sexpr.count >= 1,
	      "Passed invalid amount of arguments to 'join'!");

  for(int i = 0; i < v->value.sexpr.count; ++i) {
    LVAL_ASSERT(v, v->value.sexpr.cell[i]->type == LVAL_QEXPR,
	      "Passed a type other than a Q-Expression to 'join'!");
  }
  
  lval_t* a = lval_pop(v, 0);
  while(v->value.sexpr.count > 0) {
    a = lval_join(a, lval_pop(v, 0));
  }

  lval_delete(v);
  return a;
}

lval_t* builtin_cons(lval_t* v) {
  // check for correct arguments
  LVAL_ASSERT_ARGS_EQ(v, 2, "cons");
  
  // check argument types
  lval_t* q = v->value.sexpr.cell[1];
  LVAL_ASSERT(v, q->type == LVAL_QEXPR,
	      "Second argument of 'cons' should be a Q-Expression!");

  // do the actual work
  q = lval_qexpr();
  lval_add(q, lval_pop(v, 0));
  q = lval_join(q, lval_take(v, 0));

  return q;
}

lval_t* builtin_len(lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "len");
  v = lval_take(v, 0);
  LVAL_ASSERT(v, v->type == LVAL_QEXPR,
	      "A type other than a Q-Expression passed to 'len'!");

  lval_t* n = lval_number(v->value.sexpr.count);
  lval_delete(v);
  return n;
}

lval_t* builtin_init(lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "init");
  v = lval_take(v, 0);

  LVAL_ASSERT(v, v->type == LVAL_QEXPR,
	      "Passed an invalid type to 'v'!");
  LVAL_ASSERT(v, v->value.sexpr.count != 0,
	      "Passed an empty Q-Expression to 'init'!");

  // delete the head of the Q-Expression
  lval_delete(lval_pop(v, v->value.sexpr.count - 1));

  return v;
}

lval_t* builtin(lval_t* v, char* func) {
  if(strcmp("head", func) == 0) return builtin_head(v);
  if(strcmp("tail", func) == 0) return builtin_tail(v);
  if(strcmp("list", func) == 0) return builtin_list(v);
  if(strcmp("eval", func) == 0) return builtin_eval(v);
  if(strcmp("join", func) == 0) return builtin_join(v);
  if(strcmp("cons", func) == 0) return builtin_cons(v);
  if(strcmp("len", func) == 0) return builtin_len(v);
  if(strcmp("init", func) == 0) return builtin_init(v);
  if(strstr("+-*/%^", func)) return builtin_op(v, func);
  // At this point all hope is lost
  lval_delete(v);
  return lval_error("Unknown Function!");
}

lval_t* lval_eval_sexpr(lval_t* v) {
  // evaluate all cells
  for(int i = 0; i < v->value.sexpr.count; ++i) {
    if(v->value.sexpr.cell[i]->type == LVAL_ERROR) // check for errors 
      return lval_take(v, i);
    v->value.sexpr.cell[i] = lval_eval(v->value.sexpr.cell[i]);
  }

  // check for early returns
  if(v->value.sexpr.count == 0)
    return v; // empty expression
  if(v->value.sexpr.count == 1)
    return lval_take(v, 0); // single expression

  // ensure first element is a symbol
  lval_t* f = lval_pop(v, 0);
  if(f->type != LVAL_SYMBOL) {
    lval_delete(f);
    lval_delete(v);
    return lval_error("S-Expression does not start with a symbol!");
  }

  lval_t* result = builtin(v, f->value.symbol);
  lval_delete(f);
  return result;
}

lval_t* lval_eval(lval_t* v) {
  // evaluate s-expression
  if(v->type == LVAL_SEXPR)
    return lval_eval_sexpr(v);

  // all other types stay the same
  return v;
}

int eval_and_print(LispyParser* parser, char* input) {
  mpc_result_t result;
  if(mpc_parse("<stdin>", input, parser->lispy, &result)) {
    // on result print the AST
    lval_t* res = lval_eval(lval_read((mpc_ast_t*)result.output));
    lval_println(res);
    lval_delete(res);
    mpc_ast_delete(result.output);
    return 0;
  } else {
    // otherwise print the error
    mpc_err_print(result.error);
    mpc_err_delete(result.error);
    return 1;
  }
}

