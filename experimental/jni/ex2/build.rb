#!/usr/bin/env ruby

FILES = ["Prompt"]

SDK = "/Developer/SDKs/MacOSX10.6.sdk"
JAVA = File.join(SDK, "System/Library/Frameworks/JavaVM.framework")
HEADERS = File.join(JAVA, "Headers")

FILES.each {|f| print `javac #{f}.java`}

if $?.success?
  FILES.each {|f| print `javah -jni #{f}`}
end

if $?.success?
  FILES.each {|f| print `gcc -I#{HEADERS} -c #{f}.c`}
end

if $?.success?
  FILES.each {|f| print `gcc -dynamiclib -o lib#{f}.jnilib #{f}.o`}
end

puts "ok"
