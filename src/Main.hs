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

