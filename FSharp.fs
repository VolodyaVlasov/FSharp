let x = 2.1 + 3.2 * (8.0 / 4.0 + 1.3)

let g n = n + 5 // int -> int

let gg = fun n -> n + 5

let h (x, y) = System.Math.Sqrt(x * x + y * y)

let days_in_month =
    function
    | 1 -> 31
    | 2 -> 28
    | 3 -> 31
    | 4 -> 30
    | 5 -> 31
    | 6 -> 30
    | 7 -> 31
    | 8 -> 31
    | 9 -> 30
    | 10 -> 31
    | 11 -> 30
    | 12 -> 31
    | _ -> 0

let rec fibo =
    function
    | n when n < 1 -> 0
    | n when n <= 2 -> 1
    | n -> fibo (n - 1) + fibo (n - 2)

let rec sum =
    function
    | n when n = 0 -> 0
    | n when n = 1 -> 1
    | n -> n + sum (n - 1)

let rec sum2 =
    function
    | (m, 0) -> m
    | (m, n) -> m + n + sum2 (m, n - 1)


let notDivisible (n, m) = m % n = 0

let rec check =
    function
    | n, d when d > n / 2 -> true
    | n, d when n % d = 0 -> false
    | n, d -> check (n, d + 2)

let prime =
    function
    | 1
    | 2
    | 3 -> true
    | n when n % 2 = 0 -> false
    | n -> check (n, 3)

let rec pow =
    function
    | (s, 0) -> ""
    | (s, n) -> s + pow (s, n - 1)

let rec isIthChar = function
    | (s, n, c) when n < 0 || n >= String.length s -> false
    | (s, n, c) -> s.[n] = c

let rec occFromIth = function
    | (s, n, c) when n < 0 || n >= String.length s -> 0
    | (s, n, c) when s.[n] = c -> 1 + occFromIth(s, n + 1, c)
    | (s, n, c) -> occFromIth(s, n + 1, c)
    
// 20.3.1
let vat n x = x + x / 100.0 * (float) n    

// 20.3.2
let unvat n x = x * 100.0 / (100.0 + (float) n)
    
