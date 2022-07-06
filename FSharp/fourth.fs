let rec fibo = function
    | n when n < 1 -> 0
    | n when n <= 2 -> 1
    | n -> fibo(n - 1) + fibo(n - 2)
    
let rec sum = function
    | n when n = 0 -> 0
    | n when n = 1 -> 1
    | n -> n + sum(n - 1)
    
let rec sum2 = function 
 | (m,0) -> m 
 | (m,n) -> m + n + sum2(m, n - 1)    
    
