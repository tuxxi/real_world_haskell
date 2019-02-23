drop' :: Integer -> [a] -> [a]
drop' n xs = if n <= 0 || null xs
            then xs
            else drop' (n - 1) (tail xs)

take' :: Integer -> [a] -> [a]
take' n xs = if n <= 0 || null xs
            then []
            else (head xs : take' (n-1) (tail xs))

last' :: [a] -> a
last' (x:xs) = if length xs <= 0 || null xs
            then x
            else last' xs

-- Trying to use maybe monad
lastButOne :: [a] -> Maybe a
lastButOne (x:xs) = if length xs <= 0 || null xs
            then Nothing
            else if length xs <= 1
            then Just x
            else lastButOne xs
