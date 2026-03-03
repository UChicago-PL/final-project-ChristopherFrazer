import Data.Char (isLower, isUpper, ord, chr, isAlpha)
--import System.randomly (random)
import System.Environment (getArgs)

-- Text showing the available transformations
transformationText :: String
transformationText = "Transformations: 1 - Insertion | 2 - Deletion | 3 - Word Scramble | 4 - Character Swap"

-- Prints the rules of the game
printRules :: IO()
printRules = do
    text <- readFile "rules.txt"
    mapM_ putStrLn (lines text)

-- Function to wrap text to the specified character length by adding newlines
wrapText :: Int -> String -> String
wrapText _ [] = []
wrapText width s
    |length s <= width = s
    |otherwise = take width s ++ "\n" ++ wrapText width (drop width s)

roundText :: String -> String -> IO()
roundText original transformed = do
    putStrLn ((wrapText 60 ("Original Text: " ++ original)) ++ "\n\n" ++ (wrapText 60 ("Transformed Text: " ++ transformed ++ "\n")))

--Samples n_samp unique numbers from [0, x] uniformly, returning a list of samples
sample :: Int -> [a] -> [a]
sample 0 _ = []
sample _ [] = []
sample num_samp arr = do
    samp_ind <- random 0 (length arr)
    val <- arr !! samp_ind
    new_arr = del arr samp_ind
    sample num_samp-1


-- Function to implement the wordSwap transformation, which randomly scrambles
-- the word order of the input string.
wordSwap :: String -> String
wordSwap inp = inp

--Swaps the elements at the specified indices in the array
swap :: [a] -> Int -> Int -> [a]
swap arr i1 i2 = aux arr (min i1 i2) (max i1 i2) (arr !! (max i1 i2)) (arr !! (min i1 i2)) 0 where   
    aux :: [a] -> Int -> Int -> a -> a -> Int -> [a]
    aux [] _ _ _ _ _= []
    aux (x:xs) low_i high_i low_v high_v curr 
        |curr == low_i = low_v : aux xs low_i high_i low_v high_v (curr+1 )
        |curr == high_i = high_v : xs
        |otherwise = x : aux xs low_i high_i low_v high_v (curr+1)

--Remove the value at the given index from the list
del :: [a] -> Int -> [a]
del xs i = take i xs ++ drop (i + 1) xs


main :: IO()
main = do
    args <- getArgs
    input <- getLine
    putStrLn ("You said:" ++ input)
    roundText "There once was a story " "There once was another story "
    print (swap [0,1,2,3,4] 0 3)
    print (del [0,1,2,3,4] 0)
    main

