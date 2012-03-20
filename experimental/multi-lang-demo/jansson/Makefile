demo : demo.o
	gcc -ljansson -lcurl -o demo demo.o

demo.o : demo.c
	gcc -c $<

clean :
	rm -rf demo *.o
	