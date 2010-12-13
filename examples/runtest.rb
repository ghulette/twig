#!/usr/bin/env ruby

test = ARGV[0]

root = File.expand_path(File.dirname($0))

rules = "#{root}/rewriting/#{test}.rules"
terms = "#{root}/rewriting/#{test}.terms"

puts `#{root}/../dist/build/rewrite/rewrite #{rules} < #{terms}`
