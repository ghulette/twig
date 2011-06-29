#!/usr/bin/env ruby

FILES = ["convolution"]

SDK = "/Developer/SDKs/MacOSX10.6.sdk"
JAVA = File.join(SDK, "System/Library/Frameworks/JavaVM.framework")
HEADERS = File.join(JAVA, "Headers")
SRCDIR = "src"
OBJDIR = "build/obj"
LIBDIR = "build/lib"

`mkdir -p #{OBJDIR}`
`mkdir -p #{LIBDIR}`
FILES.each {|f| print `gcc -I#{HEADERS} -Ibuild/include -c #{SRCDIR}/#{f}.c -o #{OBJDIR}/#{f}.o`}
FILES.each {|f| print `gcc -dynamiclib -o #{LIBDIR}/lib#{f}.jnilib #{OBJDIR}/#{f}.o`}
puts "ok"
