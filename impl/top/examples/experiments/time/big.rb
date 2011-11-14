#!/usr/bin/env ruby

def succ(n)
  1.upto(n) do
    print "s("
  end
  print "z"
  1.upto(n) do
    print ")"
  end
  puts
end

n = ARGV[0]
succ(n.to_i)
