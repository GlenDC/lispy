#ifndef __LISPY_PROMPT__
#define __LISPY_PROMPT__

#include <stdio.h>
#include <stdlib.h>

#include "parsing.h"
#include <editline/readline.h>

int main(int argc, char** argv) {
  puts("Lispy version 0.0.9");
  puts("Press C-c to quit\n");

  // extra's
  // + Change env to a hash table so that we don't have to
  //   compare all the fucking strings all the time?
  // + Make macro to easier get union child (!) (!)

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
