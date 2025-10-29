main : listunit_l2.o listunit_l2.h main.c
	cc -o main main.c listunit_l2.o -fsanitize=address

listunit_l2.o : listunit_l2.c listunit_l2.h
	cc -c listunit_l2.c -fsanitize=address

clean :
	rm -f listunit_l2.o main