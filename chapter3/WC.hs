-- count words in input file

main :: IO()
main = interact wordCount
    where wordCount input = show (length input) ++ "\n"
