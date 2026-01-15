main : listunit_l1.o listunit_l1.h main.c
	cc -o main main.c listunit_l1.o -fsanitize=address

listunit_l1.o : listunit_l1.c listunit_l1.h
	cc -c listunit_l1.c -fsanitize=address

clean :
	rm -f listunit_l1.o main