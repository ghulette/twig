#!/usr/bin/env bash

javac Hello.java
javah -jni Hello
gcc -I /System/Library/Frameworks/JavaVM.framework/Headers -c Hello.c
gcc -dynamiclib -o libHello.jnilib Hello.o
