repl:
	cc -std=c11 -Wall -ledit -g -o bin/Lispy mpc/mpc.c parsing.c prompt.c
