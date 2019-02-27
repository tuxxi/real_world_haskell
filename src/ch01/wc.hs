-- file: ch01/WC.hs

inp :: IO ()
inp = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"

main :: IO ()
main = inp 