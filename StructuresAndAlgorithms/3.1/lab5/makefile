main : treeunit.o treeunit.h main.c
	cc -o main main.c treeunit.o -fsanitize=address -g

treeunit.o : treeunit.c treeunit.h
	cc -c treeunit.c -fsanitize=address -g

clean :
	rm -f treeunit.o main
