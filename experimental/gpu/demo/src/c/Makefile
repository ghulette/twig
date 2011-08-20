JNI=/Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/JavaVM.framework
INCLUDE=../../build/include
SOURCES=gpu.c convolution.c
OBJECTS=$(SOURCES:.c=.o)
TARGET=libconvolution.jnilib
CC=gcc
CFLAGS=-c -I$(JNI)/Headers -I$(INCLUDE)
LDFLAGS=-dynamiclib

all : $(SOURCES) $(TARGET)

$(TARGET) : $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@
	mkdir -p ../../build/lib
	cp $(TARGET) ../../build/lib

.c.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rf $(OBJECTS) $(TARGET)
