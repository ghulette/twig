PY_VERSION=2.7
PY_HOME=/System/Library/Frameworks/Python.framework/Versions/$(PY_VERSION)
PY_INC=$(PY_HOME)/include/python$(PY_VERSION)
PY_LIB=$(PY_HOME)/lib/python$(PY_VERSION)

call : call.o
	gcc -L$(PY_LIB) -lpython -o call call.o

call.o : call.c
	gcc -c -I$(PY_INC) call.c

clean :
	rm -rf call *.o *.pyc