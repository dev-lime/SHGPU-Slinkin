main : listunit_l2c.o listunit_l2c.h main.c
	cc -o main main.c listunit_l2c.o -fsanitize=address

listunit_l2.o : listunit_l2c.c listunit_l2c.h
	cc -c listunit_l2c.c -fsanitize=address

clean :
	rm -f listunit_l2c.o main