require 'prime'

MAX = 120000

rad = Array.new(MAX+1,1)
Prime.each {|pr|
  if pr > MAX
    break
  else
    for i in 1..(MAX/pr)
      rad[i*pr] = rad[i*pr]*pr
    end
  end
}

result = 0
for a in 1..MAX
  for b in a+1..MAX-a
    if a.gcd(b) == 1 && rad[a]*rad[b]*rad[a+b] < a+b
      puts "#{a}, #{b}, #{a+b}"
      result += a+b
    end
  end
end

p result
