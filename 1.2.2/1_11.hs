f :: Int -> Int
f n
    | n < 3 = n
    | otherwise = f (n-1) + f (n-2) * 2 + f (n - 3) * 3

f' :: Int -> Int
f' n
    | n < 3 = n
    | otherwise = iter 2 1 0 3 n
iter :: Int -> Int -> Int -> Int -> Int -> Int
iter a b c cur n
    | cur == n = res
    | otherwise = iter res a b (cur+1) n
    where res = a + 2 * b + 3 * c
main = do
    print $ f 25
    print $ f' 25
