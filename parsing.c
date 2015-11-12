#include "parsing.h"

LispyParser* new_parser() {
  LispyParser* parser = malloc(sizeof(LispyParser));

  // Create some parsers
  parser->number = mpc_new("number");
  parser->decimal = mpc_new("decimal");
  parser->operator = mpc_new("operator");
  parser->expression = mpc_new("expression");
  parser->lispy = mpc_new("lispy");

  // Define them with the following language
  mpca_lang(MPCA_LANG_DEFAULT,
    "                                                         \
      decimal: /-?[0-9]+[.][0-9]+/;                              \
      number: /-?[0-9]+/;                                     \
      operator: '+' | '-' | '*' | '/' | '%' | '^';			\
      expression: <decimal> | <number> | '(' <operator> <expression>+ ')';  \
      lispy: /^/ <operator> <expression>+ /$/;                \
    ",
    parser->number,
    parser->decimal,
    parser->operator,
    parser->expression,
    parser->lispy);

  return parser;
}

void destroy_parser(LispyParser** parser) {
  if(*parser) {
    LispyParser* _parser = *parser;
    // Undefine and Delete our parser
    mpc_cleanup(
      5,
      _parser->number,
      _parser->decimal,
      _parser->operator,
      _parser->expression,
      _parser->lispy);

    free(_parser);
    parser = NULL;
  }
}

enum lval_type {
  LVAL_NUMBER,
  LVAL_DECIMAL,
  LVAL_ERROR,
};

enum lval_error {
  LERR_DIV_ZERO,
  LERR_BAD_OPERATION,
  LERR_BAD_NUMBER,
};

typedef struct {
  enum lval_type type;
  union {
    enum lval_error error;
    long number;
    double decimal;
  } value;
} lval_t;

lval_t lval_number(long number) {
  lval_t val;
  val.type = LVAL_NUMBER;
  val.value.number = number;
  return val;
}

lval_t lval_decimal(double decimal) {
  lval_t val;
  val.type = LVAL_DECIMAL;
  val.value.decimal = decimal;
  return val;
}

lval_t lval_error(enum lval_error error) {
  lval_t val;
  val.type = LVAL_ERROR;
  val.value.error = error;
  return val;
}

void lval_print(lval_t v) {
  switch(v.type) {
    case LVAL_NUMBER:
      printf("%li", v.value.number);
      break;

    case LVAL_DECIMAL:
      printf("%f", v.value.decimal);
      break;
    
    case LVAL_ERROR:
      switch(v.value.error) {
        case LERR_DIV_ZERO:
	  printf("Error: dividing by zero");
	  break;
	  
        case LERR_BAD_OPERATION:
	  printf("Error: invalid operation");
	  break;
	  
        case LERR_BAD_NUMBER:
	  printf("Error: invalid number");
	  break;
	  
        default:
	  printf("Error: Unknown error");
	  break;
	  
      } break;

    default:
      printf("Error: Unknown lval");
      break;
  }
}

void lval_println(lval_t v) {
  lval_print(v);
  putchar('\n');
}

double get_lval_decimal(lval_t v) {
  if(v.type == LVAL_DECIMAL) return v.value.decimal;
  return (double)v.value.number;
}

#define OP_DECIMAL_IF_NEEDED(_x_, _y_, _op_) \
    if((_x_).type == LVAL_DECIMAL || (_y_).type == LVAL_DECIMAL) {	     \
      return lval_decimal(get_lval_decimal(_x_) _op_ get_lval_decimal(_y_)); \
    } else { \
      return lval_number((_x_).value.number _op_ (_y_).value.number);	\
    }

lval_t op_add(lval_t x, lval_t y) {
  OP_DECIMAL_IF_NEEDED(x, y, +)
}

lval_t op_sub(lval_t x, lval_t y) {
  OP_DECIMAL_IF_NEEDED(x, y, -)
}

lval_t op_mul(lval_t x, lval_t y) {
  OP_DECIMAL_IF_NEEDED(x, y, *)
}

lval_t op_div(lval_t x, lval_t y) {
  if(get_lval_decimal(y) == 0) {
    return lval_error(LERR_DIV_ZERO);
  }

  OP_DECIMAL_IF_NEEDED(x, y, /)
}

lval_t op_mod(lval_t x, lval_t y) {
  if(y.value.number == 0) {
    return lval_error(LERR_DIV_ZERO);
  }

  if(y.type == LVAL_DECIMAL || x.type == LVAL_DECIMAL) {
    return lval_error(LERR_BAD_NUMBER);
  }

  return lval_number(x.value.number % y.value.number);
}

lval_t op_pow(lval_t x, lval_t y) {
  double result = pow(get_lval_decimal(x), get_lval_decimal(y));
  if(x.type == LVAL_DECIMAL || y.type == LVAL_DECIMAL) {
    return lval_decimal(result);
  } else {
    return lval_number((long)result);
  }
}

typedef lval_t (*operation_t)(lval_t, lval_t);

operation_t get_op(char* contents) {
  if(strstr(contents, "+")) return op_add;
  if(strstr(contents, "-")) return op_sub;
  if(strstr(contents, "*")) return op_mul;
  if(strstr(contents, "/")) return op_div;
  if(strstr(contents, "%")) return op_mod;
  if(strstr(contents, "^")) return op_pow;

  return NULL;
}

lval_t eval_single_op(char* op, lval_t x) {
  if(strstr(op, "-")) {
    if(x.type == LVAL_NUMBER) lval_number(-x.value.number);
    return lval_decimal(-x.value.decimal);
  }

  return x;
}

lval_t eval(mpc_ast_t* node) {
  // Base Case: We find a number
  if(strstr(node->tag, "number")) {
    errno = 0;
    long x = strtol(node->contents, NULL, 10);
    return errno != ERANGE ? lval_number(x) : lval_error(LERR_BAD_NUMBER);
  }

  if(strstr(node->tag, "decimal")) {
    errno = 0;
    double x = strtod(node->contents, NULL);
    return errno != ERANGE ? lval_decimal(x) : lval_error(LERR_BAD_NUMBER);
  }

  lval_t x = eval(node->children[2]);
  operation_t op = get_op(node->children[1]->contents);
  if(!op) {
    return lval_error(LERR_BAD_OPERATION);
  }
  
  int i = 3;
  while(strstr(node->children[i]->tag, "expression")) {
    if(x.type == LVAL_ERROR) {
      return x;
    }

    lval_t y = eval(node->children[i]);
    if(y.type == LVAL_ERROR) {
      return y;
    }

    x = op(x, y);
    ++i;
  }

  if(i == 3) {
    return eval_single_op(node->children[1]->contents, x);
  }

  return x;
}

int eval_and_print(LispyParser* parser, char* input) {
  mpc_result_t result;
  if(mpc_parse("<stdin>", input, parser->lispy, &result)) {
    // on result print the AST
    lval_t res = eval((mpc_ast_t*)result.output);
    lval_println(res);
    mpc_ast_delete(result.output);
    return 0;
  } else {
    // otherwise print the error
    mpc_err_print(result.error);
    mpc_err_delete(result.error);
    return 1;
  }
}
