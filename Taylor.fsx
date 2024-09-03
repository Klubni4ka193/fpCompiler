let eps = 1e-6

let fact = 
    let rec fact' acc = function
    | n when n = 1. -> acc
    | n -> fact' (acc*n) (n - 1.)
    fact' 1.

let abs x = 
    if x >= 0. then x
    else -x

// General loop
let rec while1 exitCond act cur =
    if (exitCond cur) then cur
    else while1 exitCond act (act cur)

let power (x: float) (n: int) =
    let exitCondition (n, _, _) = n < 1
    let action (n, x, acc) = (n - 1, x, acc * x)
    let (_, _, res) = while1 exitCondition action (n, x, 1.)
    res


let taylor_naive x =
    let computeNextNaive x n =
        if n >= 1 then (power (-1.0) (n - 1)) * (power 2.0 (2*n - 1)) * (power x (2*n)) / ( fact (2.0*float(n)) )
        else 0.
    let exitCondition (_, cur, x, i) = abs(cur - (computeNextNaive x (i+1))) < eps 
    let action (acc, cur, x, i) = (acc + cur, (computeNextNaive x (i+1)), x, i+1)
    while1 exitCondition action (0.0, 0.0, x, 0)

let taylor x = 
    let computeNext u x n = 
        if n > 1 then (-1.)*(u * 4. * x * x) / (2. * float(n)) / (2. * float(n) - 1.)
        else (2. * x * x) / 2.
    let exitCondition (acc, cur, x, i) = abs(cur - (computeNext cur x (i+1))) < eps 
    let action (acc, cur, x, i) = (acc + cur, (computeNext cur x (i+1)), x, i+1)
    while1 exitCondition action (0.0, 0.0, x, 0)


// Task with built-in function
let sinInPowerOfTwo x = pown (sin x) 2

let a = 0.0
let b = 1.0
let n = 10

let main n =
    printfn "   x    Built-in      Naive iter     Smart iter"
    for i=0 to n do
        let x = a+(float i)/(float n)*(b-a)
        let (val0, _, _, s0) = taylor_naive x
        let (val1, _, _, s1) = taylor x
        printfn "%5.2f  %10.6f  %10.6f %d   %10.6f %d" x (sinInPowerOfTwo x) val0 s0 val1 s1
main n
