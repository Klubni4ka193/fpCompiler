
let eps = 1e-3

let abs x = 
    if x >= 0.0 then x
    else -x

let rec while1 exitCond act cur = 
    if (exitCond cur) then cur
    else while1 exitCond act (act cur) 

// Определите функции для решение алгебраических уравнений
let dichotomy f a b = 

    let exitCondition (a, b) = ((b - a) < eps)

    let action (a, b) = 
        let c = (a + b) / 2.0
        if (f b) * (f c) < 0.0 then (c, b)
        else (a, c)
    
    let (u, v) = while1 exitCondition action (a, b)
    (u + v) / 2.0


let iterations phi x0 = 
    let action x = phi x
    let exitCondition x = abs(x - phi x) < eps
    while1 exitCondition action x0


let newthon f f' x0 = 
    let expr x = (x - (f x / (f' x)))
    iterations expr x0

// используйте функцию 'iterations'

// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 x = 2.0*x * (sin x) - (cos x)
let f2 x = exp x + sqrt(1.0 + exp (2.0*x)) - 2.0
let f3 x = log x - x + 1.8

let f1' x = 2.0 * (sin x) + 2.0 * x * (cos x) + sin x
let f2' x = exp x + (1.0 / sqrt(1.0 + exp (2.0*x)) * (exp (2.0*x)))
let f3' x = 1.0 / x - 1.0

let phi1 x = cos x / (2.0 * (sin x))
let phi2 x = log (2.0 - sqrt(1.0 + exp (2.0*x))) 
let phi3 x = log x + 1.8


let main = 
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 0.4 1) (iterations phi1 0.7)  (newthon f1 f1' 0.7)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 -1.0 0.0) (iterations phi2 -0.5) (newthon f2 f2' -0.5)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 2.0 3.0) (iterations phi3 2.5) (newthon f3 f3' 2.5)
main 
