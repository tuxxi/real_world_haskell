import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import Data.Either (fromRight)

-- 01: use fold to improve asInt
asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold xs = foldl' step 0 xs
                    where
                step acc c = 10 * acc + digitToInt c

-- 02: use Either Monad to handle errors
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = Right 0
asInt_either ('-':xs) = fmap negate $ asInt_either xs
asInt_either ('+':xs) = asInt_either xs
asInt_either xs
                | all isDigit xs   = Right (asInt_fold xs)
                | otherwise        = Left ("Error! Non-digit in " ++ xs)
