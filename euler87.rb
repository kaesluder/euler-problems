require 'primes'
require 'set'


def maxfourth(limit)
    ((limit - 12) ** (1.0/4)).floor
end


def maxcube(limit,fourth)
    ((limit - 4 - (fourth ** 4)) ** (1.0/3)).floor
end

def maxsquare(limit,fourth,cube)
    ((limit - (fourth ** 4) - (cube ** 3)) ** (1.0/2)).floor
end

def count_e87_sums(limit)
    p = eratosthenes2((limit ** (1.0/2)).floor)
    results = Hash.new
    fourths = p.take_while {|i| i <= maxfourth(limit) }
    fourths.each do | fourth |
        cubes = p.take_while {|i| i<=maxcube(limit,fourth)}
        cubes.each do | cube |
            squares = p.take_while {|i| i<=maxsquare(limit,fourth,cube)}
            squares.each {|square| 
                results[((square ** 2) + (cube ** 3) + (fourth ** 4))] = 1}
        end
    end
    return results.size
    
end

puts count_e87_sums(50000000)
puts ""

#eratosthenes2((5000000 ** (1.0/2)))
