let notDivisible (n, m) =  m % n = 0

let rec check = function
   | n, d when d > n / 2 -> true
   | n, d when n % d = 0 -> false
   | n, d -> check(n, d + 1)
       
let  prime = function
   | 1 | 2 -> true
   | n -> check (n , 2)
