import Time

-- 1, 2) Write your own version of `length`: Take a list and return the # of elements
length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

-- tail call recursive version
lenTail :: [a] -> Integer
lenTail [] = 0
lenTail xs = helper xs 0
                where
            helper [] n = n
            helper (_:ds) n = helper ds (n + 1)

-- strict evaluation on tail call increment
lenTailStrict :: [a] -> Integer
lenTailStrict [] = 0
lenTailStrict xs = helper xs 0
                where
            helper [] n = n
            helper (_:ds) n = helper ds $! (n + 1)

testList = [1..100000]

main :: IO ()
main = do
    putStrLn "Testing len of list with naive, tail call, and builtin."
    putStrLn $ "List has max element w/ power of " ++ show (logBase 10 (last testList))
    putStrLn "1) naive length'"
    time $ length' testList `seq` return ()
    putStrLn "2) length with tail-call"
    time $ lenTail testList `seq` return ()
    putStrLn "3) length with tail-call and strict evaulation"
    time $ lenTailStrict testList `seq` return ()
    putStrLn "4) builtin length"
    time $ length testList `seq` return ()
    putStrLn "Done!"