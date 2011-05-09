#!/usr/bin/env bash

export SDK="/Developer/SDKs/MacOSX10.6.sdk"
export JAVA="$SDK/System/Library/Frameworks/JavaVM.framework"

javac Hello.java
javah -jni Hello
gcc -I$JAVA/Headers -c Hello.c
gcc -dynamiclib -o libHello.jnilib Hello.o
