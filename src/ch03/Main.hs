-- list type (recursive)
data List a = Cons a (List a) 
            | Nil
            deriving (Show)

main :: IO () 
main = do
    putStrLn "Hello, World!"