zeroToNineteen = [ "",
                "one",
                "two",
                "three",
                "four",
                "five",
                "six",
                "seven",
                "eight",
                "nine",
                "ten",
                "eleven",
                "twelve",
                "thirteen",
                "fourteen",
                "fifteen",
                "sixteen",
                "seventeen",
                "eighteen",
                "nineteen"]
                
twentyToNinety = ["twenty", 
                "thirty", 
                "forty", 
                "fifty", 
                "sixty", 
                "seventy", 
                "eighty", 
                "ninety"]
                
def writeNumber (n):
    if n < 20:
        return zeroToNineteen[n]
    elif n < 100:
        tens = n/10
        ones = n % 10
        return ''.join([twentyToNinety[tens-2],writeNumber(ones)])
    elif n < 1000:
        #if there's a remainder, add "and"
        if (n%100 ==0): 
            return zeroToNineteen[n/100] + "hundred"
        else:
            return zeroToNineteen[n/100] + "hundredand" + writeNumber(n%100)
    elif n == 1000:
        return "onethousand"
        

print sum([len(writeNumber(x)) for x in xrange(1,1001)])