import std.stdio;
import std.math;

long euler3factor(long n){
    long lastFactor = 1;
    if ((n%2) == 0) {
        n = n/2;
        lastFactor = 2;
        while ((n%2) == 0) {
            n = n/2;
        }
    } else {
        lastFactor = 1;
    }
    long factor = 3;
    float maxFactor = sqrt(cast(float)n);
    //writeln(maxFactor);
    while ((n > 1) && (factor <= (maxFactor))){
        if ((n%factor) == 0){
            n = n/factor;
            lastFactor=factor;
            while ((n%factor) == 0){
                n = n/factor;
            }
            maxFactor = sqrt(cast(float)n);
            //writeln(maxFactor);
        } 
        factor = factor + 2;
    }
    if (n == 1){
        return lastFactor;
    } else {
        return n;
    }
}

void main(){
    writeln(euler3factor(600851475143));
}
