#!/usr/bin/env ruby

test = ARGV[0]
root = File.expand_path(File.dirname($0))
rules = "#{root}/#{test}.rules"
terms = "#{root}/#{test}.terms"
cmd = "#{root}/../../dist/build/twig/twig #{rules} < #{terms}"
puts `#{cmd}`
