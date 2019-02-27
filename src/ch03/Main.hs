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

testList :: [Double]
testList = [1..100000]

testLen :: IO ()
testLen = do
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

-- 3) compute mean of list
meanList :: Integral a => [a] -> Double
meanList [] = 0
meanList (x:xs) = sum_ / length_
                    where
                sum_ = fromIntegral $ x + sum xs
                length_ = fromIntegral $ length (x:xs)

-- 4) palindrome creation
makePalindrome :: [a] -> [a]
makePalindrome xs = xs ++ (reverse' xs)
                        where
                    reverse' [] = []
                    reverse' (d:ds) = reverse' ds ++ [d]

-- 5) checking if a list is a palindrome
checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome xs = xs == reverse xs


main :: IO ()
main = do
    print "Ch 3. Exercises"
    testLen