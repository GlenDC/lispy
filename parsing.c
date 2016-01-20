
#include "parsing.h"

lenv_t* lenv_new(void);
lenv_t* lenv_copy(lenv_t*);
void lenv_delete(lenv_t*);
void lenv_add_builtins(lenv_t*);

mpc_parser_t* LISPY = NULL;

lval_t* builtin_load(lenv_t*, lval_t*);
void lval_println(lval_t*);
lval_t* lval_add(lval_t*, lval_t*);
void lval_delete(lval_t*);
lval_t* lval_sexpr();
lval_t* lval_string(char*);

LispyParser* new_parser(int argc, char* argv[]) {
  LispyParser* parser = malloc(sizeof(LispyParser));

  // Create some parsers
  parser->number = mpc_new("number");
  parser->decimal = mpc_new("decimal");
  parser->boolean = mpc_new("boolean");
  parser->symbol = mpc_new("symbol");
  parser->expr = mpc_new("expr");
  parser->sexpr = mpc_new("sexpr");
  parser->qexpr = mpc_new("qexpr");
  parser->lispy = mpc_new("lispy");
  parser->string = mpc_new("string");
  parser->comment = mpc_new("comment");

  // Define them with the following language
  mpca_lang(MPCA_LANG_DEFAULT,
     "                                                                  \
      decimal: /-?[0-9]+[.][0-9]+/;					\
      number: /-?[0-9]+/;						\
      boolean: \"true\" | \"false\";					\
      symbol: /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/;				\
      string: /\"(\\\\.|[^\"])*\"/;					\
      comment: /;[^\\r\\n]*/;						\
      sexpr: '(' <expr>* ')';						\
      qexpr: '{' <expr>* '}';						\
      expr: <decimal> | <number> | <boolean> | <string>	|		\
            <comment> | <sexpr> | <qexpr> | <symbol> ;			\
      lispy: /^/ <expr>* /$/;						\
    ",
    parser->number,
    parser->decimal,
    parser->boolean,
    parser->symbol,
    parser->sexpr,
    parser->expr,
    parser->qexpr,
    parser->lispy,
    parser->string,
    parser->comment);

  parser->env = lenv_new();
  lenv_add_builtins(parser->env);

  LISPY = parser->lispy;

  /* if we have command line arguments, use them here */
  for(int i = 1; i < argc ; ++i) {
    lval_t* args = lval_add(lval_sexpr(), lval_string(argv[i]));
    lval_t* x = builtin_load(parser->env, args);
    if(x->type == LVAL_ERROR) lval_println(x);
    lval_delete(x);
  }

  return parser;
}

void destroy_parser(LispyParser** parser) {
  if(*parser) {
    LispyParser* _parser = *parser;
    // Undefine and Delete our parser
    mpc_cleanup(
      10,
      _parser->number,
      _parser->decimal,
      _parser->boolean,
      _parser->symbol,
      _parser->expr,
      _parser->sexpr,
      _parser->qexpr,
      _parser->lispy,
      _parser->string,
      _parser->comment);

    lenv_delete(_parser->env);

    free(_parser);
    parser = NULL;
    LISPY = NULL;
  }
}

lval_t* lval_err(char* fmt, ...) {
  lval_t* v = malloc(sizeof(lval_t));
  v->type = LVAL_ERROR;

  /* create va_list and initialise it */
  va_list va;
  va_start(va, fmt);

  /* allocate 512 bytes for the arguments string */
  v->value.error = malloc(512 * sizeof(char));

  /* print our error and reallocate it to fit the real size */
  vsnprintf(v->value.error, 511, fmt, va);
  v->value.error = realloc(v->value.error,
			      sizeof(char) * (strlen(v->value.error)+1));

  /* clean up our va_list */
  va_end(va);

  return v;
}

char* ltype_name(int type) {
  switch(type) {
  case LVAL_FUNCTION: return "Function";
  case LVAL_NUMBER: return "Number";
  case LVAL_DECIMAL: return "Decimal";
  case LVAL_BOOLEAN: return "Boolean";
  case LVAL_ERROR: return "Error";
  case LVAL_SYMBOL: return "Symbol";
  case LVAL_STRING: return "String";
  case LVAL_SEXPR: return "S-Expression";
  case LVAL_QEXPR: return "Q-Expression";
  default: return "Unknown";
  }
}

#define LVAL_ASSERT(_v_, _cond_, _fmt_, ...)	\
  do {						\
    if(!(_cond_)) {				\
      lval_delete(_v_);				\
      return lval_err(_fmt_, ##__VA_ARGS__);	\
    }						\
  } while(0)

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

lval_t* lval_boolean(char value) {
  lval_t* val = malloc(sizeof(lval_t));
  val->type = LVAL_BOOLEAN;
  val->value.boolean = value;
  return val;
}

#define VAL_BOOL_TRUE lval_boolean(1)
#define VAL_BOOL_FALSE lval_boolean(0)

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

lval_t* lval_string(char* s) {
  lval_t* val = malloc(sizeof(lval_t));
  val->type = LVAL_STRING;
  val->value.string = malloc(strlen(s) + 1);
  strcpy(val->value.string, s);
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

lval_t* lval_fun(lbuiltin fun) {
  lval_t* val = malloc(sizeof(lval_t));
  val->type = LVAL_FUNCTION;
  val->value.function.builtin = fun;
  return val;
}

lval_t* lval_lambda(lval_t* formals, lval_t* body) {
  lval_t* val = malloc(sizeof(lval_t));
  val->type = LVAL_FUNCTION;
  val->value.function.builtin = NULL;
  val->value.function.env = lenv_new();
  val->value.function.formals = formals;
  val->value.function.body = body;
  return val;
}

void lval_delete(lval_t* v) {
  switch(v->type) {
    case LVAL_NUMBER:
    case LVAL_DECIMAL:
    case LVAL_BOOLEAN:
      break;

    case LVAL_FUNCTION:
      if(v->value.function.builtin == NULL) {
	lval_delete(v->value.function.formals);
	lval_delete(v->value.function.body);
	lenv_delete(v->value.function.env);
      }
      break;

    case LVAL_ERROR:
      free(v->value.error);
      break;

    case LVAL_SYMBOL:
      free(v->value.symbol);
      break;

    case LVAL_STRING:
      free(v->value.string);
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

lval_t* lval_copy(lval_t* v) {
  lval_t* x = malloc(sizeof(lval_t));
  x->type = v->type;

  switch(x->type) {
    case LVAL_NUMBER:
    case LVAL_DECIMAL:
    case LVAL_BOOLEAN:
      x->value = v->value;
      break;
  
    case LVAL_FUNCTION:
      if(v->value.function.builtin) {
	x->value.function.builtin = v->value.function.builtin;
      }
      else {
	x->value.function.builtin = NULL;
	x->value.function.env = lenv_copy(v->value.function.env);
	x->value.function.body = lval_copy(v->value.function.body);
	x->value.function.formals = lval_copy(v->value.function.formals);
      }
      break;

    case LVAL_ERROR:
      x->value.error = malloc(sizeof(char) * strlen(v->value.error));
      strcpy(x->value.error, v->value.error);
      break;

    case LVAL_SYMBOL:
      x->value.symbol = malloc(sizeof(char) * strlen(v->value.symbol));
      strcpy(x->value.symbol, v->value.symbol);
      break;

    case LVAL_STRING:
      x->value.string = malloc(sizeof(char) * strlen(v->value.string));
      strcpy(x->value.string, v->value.string);
      break;

    case LVAL_SEXPR:
    case LVAL_QEXPR:
      x->value.sexpr.count = v->value.sexpr.count;
      x->value.sexpr.cell = malloc(sizeof(lval_t) * x->value.sexpr.count);
      for (int i = 0; i < x->value.sexpr.count; ++i) {
	x->value.sexpr.cell[i] = lval_copy(v->value.sexpr.cell[i]);
      }
      break;
  }

  return x;
}

lenv_t* lenv_new(void) {
  lenv_t* env = malloc(sizeof(lenv_t));
  env->parent = NULL;
  env->count = 0;
  env->values = NULL;
  return env;
}

lenv_t* lenv_copy(lenv_t* env) {
  lenv_t* x = malloc(sizeof(lenv_t));
  x->parent = env->parent;
  x->count = env->count;
  x->values = malloc(sizeof(lenv_val_t) * x->count);
  for (int i = 0; i < x->count ; ++i) {
    lenv_val_t* val = &env->values[i];
    x->values[i].symbol = malloc(sizeof(char) * (strlen(val->symbol) + 1));
    strcpy(x->values[i].symbol, val->symbol);
    x->values[i].value = lval_copy(val->value);
  }

  return x;
}

void lenv_delete(lenv_t* env) {
  for(int i = 0; i < env->count; ++i) {
    free(env->values[i].symbol);
    lval_delete(env->values[i].value);
  }

  free(env->values);
  free(env);
}

lval_t* lenv_get(lenv_t* env, lval_t* key) {
  for(int i = 0; i < env->count; ++i) {
    if(strcmp(env->values[i].symbol, key->value.symbol) == 0) {
      return lval_copy(env->values[i].value);
    }
  }

  // try to look in the parent
  if(env->parent) {
    return lenv_get(env->parent, key);
  }

  return lval_error("unbound symbol!");
}

typedef void(*lenv_var_assignment)(lenv_t*, lval_t*, lval_t*);

int validate_name(char* name);

void lenv_put(lenv_t* env, lval_t* key, lval_t* value) {
  for(int i = 0; i < env->count; ++i) {
    // replace existing value
    if(strcmp(env->values[i].symbol, key->value.symbol) == 0) {
      lval_delete(env->values[i].value);
      env->values[i].value = lval_copy(value);
      return;
    }
  }

  // create new value
  env->values = realloc(env->values, sizeof(lenv_val_t) * (env->count+1));
  env->values[env->count].value = lval_copy(value);
  env->values[env->count].symbol = malloc(sizeof(char) * (strlen(key->value.symbol) + 1));
  strcpy(env->values[env->count].symbol, key->value.symbol);
  env->count++;
}

/**
 * Similar to lenv_put, but defines it globally rather than locally
 */
void lenv_def(lenv_t* env, lval_t* key, lval_t* value) {
  while(env->parent) env = env->parent;
  lenv_put(env, key, value);
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

lval_t* lval_read_boolean(mpc_ast_t* node) {
  if(strcmp(node->contents, "true") == 0) {
    return VAL_BOOL_TRUE;
  }
  else if(strcmp(node->contents, "false") == 0) {
    return VAL_BOOL_FALSE;
  }

  return lval_err("Tried to read a boolean, "
		  "but '%s' is not a valid value.", node->contents);
}

lval_t* lval_read_string(mpc_ast_t* node) {
  // cut of final \"
  node->contents[strlen(node->contents)-1] = 0;
  // alloc enough without taking into account the first quote
  char* unescaped = malloc(sizeof(char) * (strlen(node->contents+1) + 1));
  strcpy(unescaped, node->contents+1);
  unescaped = mpcf_unescape(unescaped);

  // create the actual string
  lval_t* string = lval_string(unescaped);
  free(unescaped);
  return string;
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
  if(strstr(node->tag, "boolean")) return lval_read_boolean(node);
  if(strstr(node->tag, "symbol")) return lval_symbol(node->contents);
  if(strstr(node->tag, "string")) return lval_read_string(node);
  
  lval_t* v = NULL;
  if(strcmp(node->tag, ">") == 0 || strstr(node->tag, "sexpr")) v = lval_sexpr();
  else if(strstr(node->tag, "qexpr")) v = lval_qexpr();
  else return NULL;

  for(int i = 0; i < node->children_num; ++i) {
    char* contents = node->children[i]->contents;
    char* tag = node->children[i]->tag;
    if(strcmp(contents, "(") == 0
       || strcmp(contents, ")") == 0
       || strcmp(contents, "{") == 0
       || strcmp(contents, "}") == 0
       || strcmp(tag, "regex") == 0
       || strstr(tag, "comment")) {
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

void lval_print_string(char* str) {
  char* escaped = malloc((strlen(str) + 1) * sizeof(char));
  strcpy(escaped, str);
  escaped = mpcf_escape(escaped);
  printf("\"%s\"", escaped);
  free(escaped);
}

void lval_print(lval_t* v) {
  switch(v->type) {
    case LVAL_NUMBER: printf("%li", v->value.number); break;
    case LVAL_DECIMAL: printf("%f", v->value.decimal); break;
    case LVAL_BOOLEAN:
      printf("%s", (v->value.boolean ? "true" : "false"));
      break;
    case LVAL_ERROR: printf("Error: %s", v->value.error); break;
    case LVAL_SYMBOL: printf("%s", v->value.symbol); break;
    case LVAL_STRING: lval_print_string(v->value.string); break;
    case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break; 
    case LVAL_QEXPR: lval_expr_print(v, '{', '}'); break;
    case LVAL_FUNCTION:
      if(v->value.function.builtin) {
	printf("<builtin>");
      }
      else {
	printf("(\\ "); lval_print(v->value.function.formals);
	putchar(' '); lval_print(v->value.function.body); putchar(')');
      } break;
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

lval_t* builtin_op(lenv_t* e, lval_t* v, char* op) {
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
  LVAL_ASSERT((_v_), (_v_)->value.sexpr.count == (_n_),	     \
	      "Invalid amount of arguments passed to '%s'. " \
	      "Got %i, expected %i.", (_fn_), 		     \
	      (_v_)->value.sexpr.count, (_n_))

#define LVAL_ASSERT_ARGS_NEMPTY(_v_, _n_,  _fn_)		   \
  LVAL_ASSERT((_v_), (_v_)->value.sexpr.count != 0,		   \
	      "No arguments passed to '%s' while it requires %i.", \
	      (_fn_), (_n_))

#define LVAL_ASSERT_TYPE_EQ(_a_, _x_, _y_, _fn_)	\
  do { char *TX = ltype_name(_x_), *TY = ltype_name(_y_); \
  LVAL_ASSERT((_a_), (_x_) == (_y_), \
	      "Passed an invalid type to '%s'. " \
	      "Got %s, expected %s.", (_fn_), \
	      TX, TY);			      \
  } while(0)

#define LVAL_ASSERT_SELF_TYPE_EQ(_a_, _t_, _fn_)	\
  do { char *TX = ltype_name((_a_)->type), *TY = ltype_name(_t_); \
  LVAL_ASSERT((_a_), (_a_)->type == (_t_), \
	      "Passed an invalid type to '%s'. " \
	      "Got %s, expected %s.", (_fn_), \
	      TX, TY);			      \
  } while(0)

#define LVAL_ASSERT_CELL_TYPE_EQ(_a_, _n_, _t_, _fn_)	\
  do { char *TX = ltype_name((_a_)->value.sexpr.cell[_n_]->type),     \
            *TY = ltype_name(_t_);				      \
  LVAL_ASSERT((_a_), (_a_)->value.sexpr.cell[_n_]->type == (_t_),     \
	      "Passed an invalid type to '%s'. " \
	      "Got %s, expected %s.", (_fn_), \
	      TX, TY); \
  } while(0)

lval_t* builtin_head(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "head");
  
  lval_t* a = lval_take(v, 0);
  
  LVAL_ASSERT_SELF_TYPE_EQ(a, LVAL_QEXPR, "head");

  // delete all elements except the head
  while(a->value.sexpr.count > 1)
    lval_delete(lval_pop(a, 1));

  return a;
}

lval_t* builtin_tail(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "tail");
  
  lval_t* a = lval_take(v, 0);
  
  LVAL_ASSERT_SELF_TYPE_EQ(a, LVAL_QEXPR, "tail");

  // delete the head of the Q-Expression
  lval_delete(lval_pop(a, 0));

  return a;
}

lval_t* lval_eval(lenv_t* e, lval_t* v);

lval_t* builtin_list(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_SELF_TYPE_EQ(v, LVAL_SEXPR, "list");
  v->type = LVAL_QEXPR;
  return v;
}

lval_t* builtin_eval(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "eval");
  
  lval_t* a = lval_take(v, 0);
  LVAL_ASSERT_SELF_TYPE_EQ(a, LVAL_QEXPR, "eval");

  a->type = LVAL_SEXPR;
  return lval_eval(e, a);
}

lval_t* lval_join(lval_t* a, lval_t* b) {
  while(b->value.sexpr.count > 0)
    lval_add(a, lval_pop(b, 0));

  lval_delete(b);
  return a;
}

lval_t* builtin_join(lenv_t* e, lval_t* v) {
  LVAL_ASSERT(v, v->value.sexpr.count >= 1,
	      "Passed invalid amount of arguments to 'join'. "
	      "Got %i, expected at least 1 arguments.",
	      v->value.sexpr.count);

  for(int i = 0; i < v->value.sexpr.count; ++i) {
    LVAL_ASSERT_CELL_TYPE_EQ(v, i, LVAL_QEXPR, "join");
  }
  
  lval_t* a = lval_pop(v, 0);
  while(v->value.sexpr.count > 0) {
    a = lval_join(a, lval_pop(v, 0));
  }

  lval_delete(v);
  return a;
}

lval_t* builtin_cons(lenv_t* e, lval_t* v) {
  // check for correct arguments
  LVAL_ASSERT_ARGS_EQ(v, 2, "cons");
  
  // check argument types
  lval_t* q = v->value.sexpr.cell[1];
  LVAL_ASSERT_CELL_TYPE_EQ(v, 1, LVAL_QEXPR, "cons");

  // do the actual work
  q = lval_qexpr();
  lval_add(q, lval_pop(v, 0));
  q = lval_join(q, lval_take(v, 0));

  return q;
}

lval_t* builtin_len(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "len");
  v = lval_take(v, 0);
  LVAL_ASSERT_SELF_TYPE_EQ(v, LVAL_QEXPR, "len");

  lval_t* n = lval_number(v->value.sexpr.count);
  lval_delete(v);
  return n;
}

lval_t* builtin_init(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "init");
  v = lval_take(v, 0);

  LVAL_ASSERT_SELF_TYPE_EQ(v, LVAL_QEXPR, "init");

  // delete the head of the Q-Expression
  lval_delete(lval_pop(v, v->value.sexpr.count - 1));

  return v;
}

lval_t* builtin_add(lenv_t* e, lval_t* v) {
  return builtin_op(e, v, "+");
}

lval_t* builtin_sub(lenv_t* e, lval_t* v) {
  return builtin_op(e, v, "-");
}

lval_t* builtin_mul(lenv_t* e, lval_t* v) {
  return builtin_op(e, v, "*");
}

lval_t* builtin_div(lenv_t* e, lval_t* v) {
  return builtin_op(e, v, "/");
}

lval_t* builtin_mod(lenv_t* e, lval_t* v) {
  return builtin_op(e, v, "%");
}

lval_t* builtin_pow(lenv_t* e, lval_t* v) {
  return builtin_op(e, v, "^");
}

typedef char(*order_func)(long double, long double);

#define ORDER_FUNC(_PREFIX_, _OPERATOR_)		  \
  char order_ ## _PREFIX_ (long double x, long double y) { \
    return x _OPERATOR_ y;				  \
  }

ORDER_FUNC(lt, <)
ORDER_FUNC(gt, >)
ORDER_FUNC(lteq, <=)
ORDER_FUNC(gteq, >=)

lval_t* builtin_order(lenv_t* e, lval_t* v, char* func, order_func op) {
  LVAL_ASSERT_ARGS_EQ(v, 2, func);

  // make sure the types are correct
  long double values[2];
  for(int i = 0; i < 2; ++i) {
    lval_t* cell = v->value.sexpr.cell[i];
    int type = cell->type;
    LVAL_ASSERT(v, type == LVAL_NUMBER || type == LVAL_DECIMAL,
		"Invalid argument (%i) passed to '%s'. "
		"Got %s, expected %s or %s.",
		i, func, ltype_name(type),
		ltype_name(LVAL_NUMBER), ltype_name(LVAL_DECIMAL));
    values[i] = (long double)
      (cell->type == LVAL_NUMBER ? cell->value.number : cell->value.decimal);
  }

  char result = op(values[0], values[1]);
  lval_delete(v);
  return lval_boolean(result);
}

lval_t* builtin_lt(lenv_t* e, lval_t* v) {
  return builtin_order(e, v, "<", order_lt);
}

lval_t* builtin_gt(lenv_t* e, lval_t* v) {
  return builtin_order(e, v, ">", order_gt);
}

lval_t* builtin_lteq(lenv_t* e, lval_t* v) {
  return builtin_order(e, v, "<=", order_lteq);
}

lval_t* builtin_gteq(lenv_t* e, lval_t* v) {
  return builtin_order(e, v, ">=", order_gteq);
}

lval_t* builtin_not(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "not");
  LVAL_ASSERT_CELL_TYPE_EQ(v, 0, LVAL_BOOLEAN, "not");

  char result = (v->value.sexpr.cell[0]->value.boolean == 0);
  lval_delete(v);
  return lval_boolean(result);
}

lval_t* builtin_and(lenv_t* e, lval_t* v) {
  LVAL_ASSERT(v, v->value.sexpr.count >= 2,		     \
	      "Invalid amount of arguments passed to 'and'. " \
	      "Got %i, expected at least 2.", 	     \
	      v->value.sexpr.count);

  // check if they are all boolean
  // if they are we also short - circuite
  lval_t* result = NULL;
  for(int i = 0; i < v->value.sexpr.count; ++i) {
    if(v->value.sexpr.cell[i]->type != LVAL_BOOLEAN) {
      result = lval_err("Invalid argument (%i) type passed to 'and'. "
			"Got %s, expected %s.", i,
			ltype_name(v->value.sexpr.cell[i]->type),
			ltype_name(LVAL_BOOLEAN));
      break;
    }

    if(v->value.sexpr.cell[i]->value.boolean == 0) {
      result = VAL_BOOL_FALSE;
      break;
    }
  }

    if(result == NULL) {
      result = VAL_BOOL_TRUE;
    }
  
  lval_delete(v);
  return result;
}

lval_t* builtin_or(lenv_t* e, lval_t* v) {
  LVAL_ASSERT(v, v->value.sexpr.count >= 2,		     \
	      "Invalid amount of arguments passed to 'or'. " \
	      "Got %i, expected at least 2.", 	     \
	      v->value.sexpr.count);

  // check if they are all boolean
  // if they are we also short - circuite
  lval_t* result = NULL;
  for(int i = 0; i < v->value.sexpr.count; ++i) {
    if(v->value.sexpr.cell[i]->type != LVAL_BOOLEAN) {
      result = lval_err("Invalid argument (%i) type passed to 'or'. "
			"Got %s, expected %s.", i,
			ltype_name(v->value.sexpr.cell[i]->type),
			ltype_name(LVAL_BOOLEAN));
      break;
    }

    if(v->value.sexpr.cell[i]->value.boolean != 0) {
      result = VAL_BOOL_TRUE;
      break;
    }
  }

    if(result == NULL) {
      result = VAL_BOOL_FALSE;
    }
  
  lval_delete(v);
  return result;
}

int lval_eq(lval_t* x, lval_t* y) {
  switch(x->type) {
    case LVAL_NUMBER:
      return x->value.number == y->value.number;

    case LVAL_DECIMAL:
      return x->value.decimal == y->value.decimal;

    case LVAL_BOOLEAN:
      return x->value.boolean == y->value.boolean;

    case LVAL_SYMBOL:
      return strcmp(x->value.symbol, y->value.symbol) == 0;

    case LVAL_ERROR:
      return strcmp(x->value.error, y->value.error) == 0;

    case LVAL_SEXPR:
    case LVAL_QEXPR:
      if(x->value.sexpr.count != y->value.sexpr.count) return 0;
      else {
	for(int i = 0; i < x->value.sexpr.count; ++i) {
	  if(!lval_eq(x->value.sexpr.cell[i], y->value.sexpr.cell[i])) {
	    return 0;
	  }
	}
	return 1;
      }

    case LVAL_FUNCTION:
      if(x->value.function.builtin || y->value.function.builtin) {
	return x->value.function.builtin == y->value.function.builtin;
      }
      else {
	return lval_eq(x->value.function.formals, y->value.function.formals)
	  && lval_eq(x->value.function.body, y->value.function.body);
      }

    default:
      return 0;
  }
}

lval_t* builtin_cmp(lenv_t* e, lval_t* v, char* func) {
  LVAL_ASSERT_ARGS_EQ(v, 2, func);

  // check if the types are compareble
  lval_t* x = v->value.sexpr.cell[0];
  lval_t* y = v->value.sexpr.cell[1];
  LVAL_ASSERT(v, x->type == y->type,
	      "Unexpected combination of types in '%s'. "
	      "%s and %s are incomprable.", func,
	      ltype_name(x->type), ltype_name(y->type));

  char result = (char) lval_eq(x, y);
  lval_delete(v);

  if(strcmp(func, "!=") == 0) {
    result = result == 0;
  }
  
  return lval_boolean(result);
}

lval_t* builtin_eq(lenv_t* e, lval_t* v) {
  return builtin_cmp(e, v, "==");
}

lval_t* builtin_neq(lenv_t* e, lval_t* v) {
  return builtin_cmp(e, v, "!=");
}

lval_t* builtin_if(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 3, "if");
  LVAL_ASSERT_CELL_TYPE_EQ(v, 0, LVAL_BOOLEAN, "if");
  LVAL_ASSERT_CELL_TYPE_EQ(v, 1, LVAL_QEXPR, "if");
  LVAL_ASSERT_CELL_TYPE_EQ(v, 2, LVAL_QEXPR, "if");

  lval_t* value = NULL;
  if(v->value.sexpr.cell[0]->value.boolean) {
    value = lval_take(v, 1);
  } else {
    value = lval_take(v, 2);
  }

  value->type = LVAL_SEXPR;
  return lval_eval(e, value);
}

lval_t* builtin_var(lenv_t* e, lval_t* v, char* func, lenv_var_assignment put) {
  LVAL_ASSERT(v, v->value.sexpr.count >= 2,
	      "Invalid amount of arguments passed to '%s'. "
	      "Got %i, expected at least 2.", func, v->value.sexpr.count);
  LVAL_ASSERT_CELL_TYPE_EQ(v, 0, LVAL_QEXPR, func);

  /* First argument is Symbol List */
  lval_t* symbols = v->value.sexpr.cell[0];

  /* Make sure that each value in the list is actually a Symbol
     and that they are not reserved keyword */
  for(int i = 0; i < symbols->value.sexpr.count; ++i) {
    LVAL_ASSERT_TYPE_EQ(v, symbols->value.sexpr.cell[i]->type, LVAL_SYMBOL, func);
    LVAL_ASSERT(v, validate_name(symbols->value.sexpr.cell[i]->value.symbol),
	        "Tried to define a variable with a reserved keyword as the name!");
  }

  /* Check if number of values equals number of arguments passed */
  LVAL_ASSERT(v, symbols->value.sexpr.count == (v->value.sexpr.count-1),
		 "Function %s cannot define incorrect"
	         "number of values to synbols", func);

  /* Assign copies of values to symbols */
  for(int i = 0; i < symbols->value.sexpr.count; ++i) {
    put(e, symbols->value.sexpr.cell[i], v->value.sexpr.cell[i+1]);
  }

  lval_delete(v);
  return lval_sexpr();
}

lval_t* builtin_put(lenv_t* e, lval_t* v) {
  return builtin_var(e, v, "=", lenv_put);
}

lval_t* builtin_def(lenv_t* e, lval_t* v) {
  return builtin_var(e, v, "def", lenv_def);
}

lval_t* builtin_print_env(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 0, "print-env");  
  
  for(int i = 0; i < e->count; ++i) {
    printf("%s -> ", e->values[i].symbol);
    lval_print(e->values[i].value);
    printf(", ");
  }

  return lval_sexpr();
}

lval_t* builtin_typeof(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "typeof");

  char* t = ltype_name(v->value.sexpr.cell[0]->type);
  lval_t* string = lval_string(t);
  lval_delete(v);

  return string;
}

lval_t* builtin_exit(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 0, "exit");  
  exit(0);
  return lval_sexpr();
}

lval_t* builtin_lambda(lenv_t* e, lval_t* v) {
  // Make sure that we only have 2 QEXPR children
  LVAL_ASSERT_ARGS_EQ(v, 2, "\\");
  LVAL_ASSERT_CELL_TYPE_EQ(v, 0, LVAL_QEXPR, "\\");
  LVAL_ASSERT_CELL_TYPE_EQ(v, 1, LVAL_QEXPR, "\\");

  lval_t* formals = v->value.sexpr.cell[0];
  for(int i = 0; i < formals->value.sexpr.count; ++i) {
    LVAL_ASSERT_TYPE_EQ(v, formals->value.sexpr.cell[i]->type, LVAL_SYMBOL, "\\");
  }

  formals = lval_pop(v, 0);
  lval_t* body = lval_pop(v, 0);
  lval_delete(v);

  return lval_lambda(formals, body);
}

lval_t* builtin_load(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "load");
  LVAL_ASSERT_CELL_TYPE_EQ(v, 0, LVAL_STRING, "load");

  /* Parse File given in string name */
  mpc_result_t r;
  if(mpc_parse_contents(v->value.sexpr.cell[0]->value.string, LISPY, &r)) {
    /* read contents */
    lval_t* expr = lval_read(r.output);
    mpc_ast_delete(r.output);

    /* evaluate each expression */
    while(expr->value.sexpr.count) {
      lval_t* x = lval_eval(e, lval_pop(expr, 0));
      /* if error, print it */
      if(x->type == LVAL_ERROR) lval_println(x);
      lval_delete(x);
    }

    /* delete expression tree and arguments */
    lval_delete(expr);
    lval_delete(v);

    /* return empty list */
    return lval_sexpr();
  }

  /* Get parse error as string */
  char* err_msg = mpc_err_string(r.error);
  mpc_err_delete(r.error);

  /* Create and return new error message using it */
  lval_t* error = lval_err("Could not load library %s", err_msg);
  free(err_msg);
  lval_delete(v);
  return error;
}

lval_t* builtin_print(lenv_t* e, lval_t* v) {
  // print each argument followed by a space
  for(int i = 0; i < v->value.sexpr.count; ++i) {
    lval_print(v->value.sexpr.cell[i]);
    putchar(' ');
  }

  putchar('\n');
  lval_delete(v);

  return lval_sexpr();
}

lval_t* builtin_error(lenv_t* e, lval_t* v) {
  LVAL_ASSERT_ARGS_EQ(v, 1, "error");
  LVAL_ASSERT_CELL_TYPE_EQ(v, 0, LVAL_STRING, "error");

  lval_t* error = lval_err(v->value.sexpr.cell[0]->value.string);
  lval_delete(v);

  return error;
}

int validate_name(char* name) {
  char const* reserved[] = {
    "<", ">", "<=", ">=", "==", "!=", "if",
    "not", "and", "or",
    "+", "-", "/", "*", "%", "&", "^",
    "def", "=", "\\", "print-env", "exit",
    "true", "false", "typeof"
  };
  int const size = sizeof(reserved) / sizeof(char*);

  for(int i = 0; i < size; ++i) {
    if (strcmp(reserved[i], name) == 0) {
      return 0;
    }
  }

  return 1;
}

void lenv_add_variable(lenv_t* e, char* name, lval_t* value) {
  lval_t* key = lval_symbol(name);
  lenv_put(e, key, value);
  lval_delete(key);
  lval_delete(value);
}

void lenv_add_builtin(lenv_t* e, char* name, lbuiltin fun) {
  lenv_add_variable(e, name, lval_fun(fun));
}

void lenv_add_builtins(lenv_t* e) {
  /* List Functions */
  lenv_add_builtin(e, "head", builtin_head);
  lenv_add_builtin(e, "tail", builtin_tail);
  lenv_add_builtin(e, "list", builtin_list);
  lenv_add_builtin(e, "eval", builtin_eval);
  lenv_add_builtin(e, "join", builtin_join);
  lenv_add_builtin(e, "cons", builtin_cons);
  lenv_add_builtin(e, "len", builtin_len);
  lenv_add_builtin(e, "init", builtin_init);

  /* Mathematical Functions */
  lenv_add_builtin(e, "+", builtin_add);
  lenv_add_builtin(e, "-", builtin_sub);
  lenv_add_builtin(e, "*", builtin_mul);
  lenv_add_builtin(e, "/", builtin_div);
  lenv_add_builtin(e, "%", builtin_mod);
  lenv_add_builtin(e, "^", builtin_pow);

  /* Logical Functions */
  lenv_add_builtin(e, "not", builtin_not);
  lenv_add_builtin(e, "and", builtin_and);
  lenv_add_builtin(e, "or", builtin_or);
  lenv_add_builtin(e, "<", builtin_lt);
  lenv_add_builtin(e, ">", builtin_gt);
  lenv_add_builtin(e, "<=", builtin_lteq);
  lenv_add_builtin(e, ">=", builtin_gteq);
  lenv_add_builtin(e, "==", builtin_eq);
  lenv_add_builtin(e, "!=", builtin_neq);
  lenv_add_builtin(e, "if", builtin_if);

  /* User Functions */
  lenv_add_builtin(e, "def", builtin_def);
  lenv_add_builtin(e, "=", builtin_put);
  lenv_add_builtin(e, "\\", builtin_lambda);

  /* I/O Functions */
  lenv_add_builtin(e, "load", builtin_load);
  lenv_add_builtin(e, "print", builtin_print);

  /* Error Functions */
  lenv_add_builtin(e, "error", builtin_error);

  /* Debug Functions */
  lenv_add_builtin(e, "print-env", builtin_print_env);
  lenv_add_builtin(e, "typeof", builtin_typeof);
  lenv_add_builtin(e, "exit", builtin_exit);
}

lval_t* lval_call(lenv_t* e, lval_t* f, lval_t* a) {
  /* When builtin is defined, simply call it */
  if(f->value.function.builtin) {
    return f->value.function.builtin(e, a);
  }

  int given = a->value.sexpr.count;
  int total = f->value.function.formals->value.sexpr.count;

  /* While arguments still remain to be processed */
  while(a->value.sexpr.count) {
    if(f->value.function.formals->value.sexpr.count == 0) {
      lval_delete(a);
      return lval_err(
	"Function passed too many arguments. "
	"Got %i, expected %i.", given, total);
    }

    lval_t* symbol = lval_pop(f->value.function.formals, 0);

    /* Special case for the '&' symbol */
    if(strcmp(symbol->value.symbol, "&") == 0) {
      /* Make sure we actually have one argument left */
      if(f->value.function.formals->value.sexpr.count != 1) {
	lval_delete(a);
	return lval_err("Function format invalid. "
		        "Symbol '&' is not followed by single symbol.");
      }

      lval_t* vsymbol = lval_pop(f->value.function.formals, 0);
      lenv_put(f->value.function.env, vsymbol, builtin_list(e, a));
      lval_delete(symbol); lval_delete(vsymbol);
      break;
    }
    
    lval_t* value = lval_pop(a, 0);

    lenv_put(f->value.function.env, symbol, value);
    lval_delete(symbol);
    lval_delete(value);
  }

  /* Arguments have been consumed and can be cleaned up */
  lval_delete(a);

  if(f->value.function.formals->value.sexpr.count > 0
     && strcmp(f->value.function.formals->value.sexpr.cell[0]->value.symbol, "&") == 0) {
    if(f->value.function.formals->value.sexpr.count != 2) {
	return lval_err("Function format invalid. "
		        "Symbol '&' is not followed by single symbol.");
    }

    lval_delete(lval_pop(f->value.function.formals, 0));

    lval_t* symbol = lval_pop(f->value.function.formals, 0);
    lval_t* value = lval_qexpr();

    lenv_put(f->value.function.env, symbol, value);
    lval_delete(symbol); lval_delete(value);
  }

  /* if all formals have been evaluated, we can evaluate */
  if(f->value.function.formals->value.sexpr.count == 0) {
    f->value.function.env->parent = e;
    return builtin_eval(
      f->value.function.env,
      lval_add(lval_sexpr(), lval_copy(f->value.function.body)));
  }
  
  /* otherwise return */
  return lval_copy(f);
}

lval_t* lval_eval_sexpr(lenv_t* e, lval_t* v) {
  /* evaluate all cells */
  for(int i = 0; i < v->value.sexpr.count; ++i) {
    v->value.sexpr.cell[i] = lval_eval(e, v->value.sexpr.cell[i]);
  }

  for(int i = 0; i < v->value.sexpr.count; ++i) {
    if(v->value.sexpr.cell[i]->type == LVAL_ERROR) // check for errors 
      return lval_take(v, i);
  }

  /* check for early returns */
  if(v->value.sexpr.count == 0)
    return v; // empty expression
  if(v->value.sexpr.count == 1
     && v->value.sexpr.cell[0]->type != LVAL_FUNCTION)
    return lval_take(v, 0); // single expression

  /* ensure first element is a function */
  lval_t* f = lval_pop(v, 0);
  if(f->type != LVAL_FUNCTION) {
    lval_delete(f);
    lval_delete(v);
    return lval_error("first element is not a function!");
  }

  lval_t* result = lval_call(e, f, v);
  lval_delete(f);
  return result;
}

lval_t* lval_eval(lenv_t* e, lval_t* v) {
  if(v->type == LVAL_SYMBOL) {
    lval_t* x = lenv_get(e, v);
    lval_delete(v);
    return x;
  }
  
  /* evaluate s-expression */
  if(v->type == LVAL_SEXPR)
    return lval_eval_sexpr(e, v);

  /* all other types stay the same */
  return v;
}

int eval_and_print(LispyParser* parser, char* input) {
  mpc_result_t result;
  if(mpc_parse("<stdin>", input, parser->lispy, &result)) {
    // on result print the AST
    lval_t* res = lval_eval(parser->env, lval_read((mpc_ast_t*)result.output));
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
