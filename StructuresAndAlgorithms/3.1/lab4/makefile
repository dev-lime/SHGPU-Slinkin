main : squnit.o squnit.h main.c
	cc -o main main.c squnit.o -fsanitize=address

squnit.o : squnit.c squnit.h
	cc -c squnit.c -fsanitize=address

clean :
	rm -f squnit.o main