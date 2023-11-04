function sum_limit_step(limit,step) {
    var result = 0
    for (i = step; i < limit; i += step) {
        result += i;
    }
    return result;
}

function euler1(x, y, limit) {
    return (sum_limit_step(limit,x) +
            sum_limit_step(limit,y) -
            sum_limit_step(limit, (x*y)))
}

WScript.echo(euler1(3,5,1000));
//WScript.quit();