-- zipper [] [] = []
-- zipper [] _ = []
-- zipper _ [] = []
-- zipper (x:xs) (y:ys) = (x,y) : zipper xs ys
zipper xs ys = aux xs ys []
    where
        aux [] [] res = res
        aux [] _ res = res
        aux _ [] res = res
        aux (x:xs) (y:ys) res = aux xs ys (res++[(x,y)])

-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib(n-1) + fib(n-2)
fib n = aux 0 n []
    where  
        aux i n dp
            | (n == 0 || n == 1) = n
            | (i == 0 || i == 1) = aux(i+1) n (dp ++ [i])
            | i <= n = aux (i+1) n (dp ++ [(dp!!(i-1) + dp!!(i-2))])
            | i > n = dp!!(i-1)
            | otherwise = error "something went wrong"


-- list_map _ [] = []
-- list_map fn (x:xs) = (fn x) : list_map fn xs 
list_map fn l = aux fn l []
    where 
        aux _ [] res = res
        aux fn (x:xs) res = aux (fn) (xs) (res++[fn x])
-- list_map :: (t -> a) -> [t] -> [a]
