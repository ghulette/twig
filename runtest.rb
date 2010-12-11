#!/usr/bin/env ruby

test = ARGV[0]

rules = "examples/rewriting/#{test}.rules"
terms = "examples/rewriting/#{test}.terms"

puts `./dist/build/rewrite/rewrite #{rules} < #{terms}`
