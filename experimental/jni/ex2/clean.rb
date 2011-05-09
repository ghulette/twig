#!/usr/bin/env ruby

files = ["Prompt"]

headers = files.map {|f| f + ".h"}.join(' ')

print `rm -rf *.o *.class *.jnilib #{headers}`
