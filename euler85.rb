#! /usr/bin/ruby

def combinations(x, y)
    return (x * (x + 1) * y * (y + 1))/4
end

#puts(combinations(2000,1))

def e85loop(x, y, target)
    best=0
    bestx = x
    besty = y
    until (y > x)
        newvalue = combinations(x,y)
        if (target - newvalue).abs < (target - best).abs
            best = newvalue
            bestx = x
            besty = y
        end
        if (newvalue > target)
            x = x - 1
        else
            y = y + 1
        end
    
    end
    return [bestx,besty,bestx*besty]
end

puts e85loop(200,1,2000000).inspect




