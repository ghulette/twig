OBJ=hello.o

CC=gcc
LDFLAGS=-framework OpenCL

hello : $(OBJ)
	gcc -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) -c $< $(CFLAGS)

clean :
	rm -f hello *.o
