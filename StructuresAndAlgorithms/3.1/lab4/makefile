main : main.c squnitA.o squnitL.o
	cc -o main main.c squnitA.o squnitL.o -fsanitize=address -g

squnitA.o : squnitA.c squnitA.h
	cc -c squnitA.c -fsanitize=address -g

squnitL.o : squnitL.c squnitL.h
	cc -c squnitL.c -fsanitize=address -g

clean :
	rm -f squnitA.o squnitL.o main
