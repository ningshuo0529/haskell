--f::[Int]->Int->[Int]
f xs y =
    let a = [sum idx | idx <-[0..length xs -1]]
        sum idx = if idx >=y then (xs !! idx) + (a !! (idx - y)) else (xs !! idx)
    in a

--change :: [Int] -> Int -> Int
change coins total =
    let mem = [num x|x<-[0..total], let num x = if x == 0 then 1 else 0]
        res = rec (tail coins) (f mem (head coins))
    in res !! (length res - 1)
--rec::[Int] -> [Int] -> [Int]
rec [] x = x
rec x [] = []
rec coins res = rec (tail coins) (f res (head coins))

main = do
    print $ change [1,5,10,25,50] 125
