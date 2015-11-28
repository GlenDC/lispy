#ifndef __LISPY_PROMPT__
#define __LISPY_PROMPT__

#include <stdio.h>
#include <stdlib.h>

#include "parsing.h"
#include <editline/readline.h>

int main(int argc, char** argv) {
  puts("Lispy version 0.0.5");
  puts("Press C-c to quit\n");

  LispyParser* lispy = new_parser();
  
  while(1) {
    char* input = readline("Lispy> ");
    add_history(input);
    eval_and_print(lispy, input);
    free(input);
  }

  destroy_parser(&lispy);

  return 0;
}

#endif // __LISPY_PROMPT__
