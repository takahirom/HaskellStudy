module Main where
    main = do
        -- call function
        print $ doubleUs 1 2
        print $ doubleSmallNumber 40

        let lostNumbers = [4,8,15,16,23,42]
        -- get index 2
        print $ lostNumbers !! 2
        print $ head lostNumbers
        -- is null?
        print $ null []
        -- unlimited list take first 24
        print $ take 24 [13,26..]
        -- [0,2,..,20]
        print $ [x*2 | x <- [1..10]]
        -- [12,..,20]
        print $ [x*2 | x <- [1..10], x*2 >= 12]
        print $ boomBangs [1..100]
        print $ length' [1..100]
        print $ [(x,y)|x<-[1..10],y<-[1..10],z<-[1..10],z*z==x*x+y*y,24==x+y+z]
        print $  length' [1..100]
        print $  lucky 100
        print $ head' [1..4]
        print $ maxord 1 2
        print $ "1:" ++ show(wheremax [1..100])
        print $ "3:" ++ show(wheremax [1,2])
        print $ "4:" ++ show(wheremax [2,1])
        print $ "4:" ++ show(wheremax [2,3,1])

        print $ "fib:" ++ show(fib 4)
        print $ "max:" ++ show(maximum' [1,3,5,6,2,3])
        print $ "replicate:" ++ show(replicate' 3 5)
        print $ show([1,5,2,4]!! 1)
        print $ "take:" ++ show(take' 3 [1,5,2,4])
        print $ "reverse:" ++ show(reverse' [1,5,2,4])
        --print $ "repeat:" ++ show(repeat' [1,5,2,4])
        print $ "take:" ++ show(take' 3 (repeat [3]))

    repeat' :: [a] -> [a]
    repeat' x = x ++ repeat' x


    reverse' :: [a] -> [a]
    reverse' (x:xs) = reverse xs ++ [x]

    take' :: Int -> [a] -> [a]
    take' 1 y = [y !! 0]
    take' x y = (take' (x-1) y) ++ [y !! (x-1)]

    fib :: Int -> Int
    fib 0 = 1
    fib 1 = 1
    fib x = fib(x-1)+fib(x-2)

    maximum' :: (Ord a) => [a] -> a
    maximum' (x:y:[]) = if x > y then x else y
    maximum' (x:y) = if x > maximum' y then x else maximum' y

    replicate' :: Int -> a -> [a]
    replicate' 0 y = [y]
    replicate' x y = [y] ++ replicate (x-1) y

    wheremax :: (Ord a) => [a] -> a
    wheremax (x:y:[])
        | x > y = x
        | otherwise = y
    wheremax (x:y)
        | x > maxy = x
        | otherwise = maxy
        where maxy = wheremax y


    maxord :: (Ord a) => a -> a -> a
    maxord a b
        | a > b = a
        | otherwise = b


    head' :: [a] -> a
    head' (x:_) = x

    lucky :: Int -> Int
    lucky 1 = 1
    lucky x = lucky (x - 1) + x


    doubleUs x y = doubleMe x + doubleMe y
    doubleMe x = x + x
    doubleSmallNumber x = if x > 100
                            then x
                            else doubleMe x
    boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
    length' xs = sum [1 | _ <- xs]

