f row col
    | col <= 0 = 1
    | col >= row = 1
    | otherwise = f (row-1) (col-1) + f (row-1) col

f' row col = fac row / (fac col * fac (row - col))
fac x = foldl (*)  1 [1..x]

main = do
    print $ f 4 0
    print $ f 4 1
    print $ f 4 2
    print $ f 4 3
    print $ f 4 4
    print $ f 20 10
    print $ f' 20 10
