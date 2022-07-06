let rec fibo = function
    | n when n < 1 -> 0
    | n when n <= 2 -> 1
    | n -> fibo(n - 1) + fibo(n - 2)
    
    
