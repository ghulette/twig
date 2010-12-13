#!/usr/bin/env ruby

rules = "ackerman.rules"
rewrite = "../dist/build/rewrite/rewrite"

def succ(n)
  if n == 0 then "z"
  else "s(" + succ(n - 1) + ")"
  end
end

1.upto(8) do |n|
  File.open("ack#{n}.in","w") do |out|
    out.puts "-- ack(3,#{n}) = #{2**(n+3)-3}"
    out.puts "ack(#{succ(3)},#{succ(n)})"
  end
  puts `time #{rewrite} #{rules} < ack#{n}.in > ack#{n}.out`
end
