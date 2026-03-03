module Main where

import Data.Char (isLower, isUpper, ord, chr, isAlpha)
import System.Random (randomR, StdGen, getStdGen, newStdGen)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

-- Functions to Print Static Messages

-- Prints the rules of the game
printGameRules :: IO()
printGameRules = do
    text <- readFile "rules.txt"
    mapM_ putStrLn (lines text)

--Prints input error text
printInputError :: IO()
printInputError = do
    text <- readFile "inputs.txt"
    mapM_ putStrLn (lines text)

--Prints Transformatin rule text
printTransformationRules :: IO()
printTransformationRules = do
    text <- readFile "transformations.txt"
    mapM_ putStrLn (lines text)

-- General Helper Functions

--Extracts a 20-word excerpt from the input text
loadText :: String -> StdGen -> String
loadText text gen = out where
    word_lst = words text
    (excerpt_ind, gen2) = randomR (0, length word_lst-21) gen
    words_20 = take 20 (drop excerpt_ind word_lst)
    out = unwords words_20

-- Function to wrap text to the specified character length by adding newlines
wrapText :: Int -> String -> String
wrapText _ [] = []
wrapText width s
    |length s <= width = s
    |otherwise = take width s ++ "\n" ++ wrapText width (drop width s)

wrapWords :: Int -> String -> String
wrapWords width text = go (words text) ""
  where
    go [] currentLine = currentLine
    go (w:ws) "" = go ws w
    go (w:ws) currentLine
        | length currentLine + 1 + length w <= width =
            go ws (currentLine ++ " " ++ w)
        | otherwise =
            currentLine ++ "\n" ++ go ws w


--Swaps the elements at the specified indices in the array
swap :: [a] -> Int -> Int -> [a]
swap arr i1 i2 = aux arr min_ind max_ind l_v h_v 0 where
    last_i  = length arr - 1
    max_ind = min (max i1 i2) last_i
    min_ind = max (min i1 i2) 0
    l_v = arr !! max_ind
    h_v = arr !! min_ind
    aux :: [a] -> Int -> Int -> a -> a -> Int -> [a]
    aux [] _ _ _ _ _= []
    aux (x:xs) low_i high_i low_v high_v curr 
        |curr == low_i = low_v : aux xs low_i high_i low_v high_v (curr+1)
        |curr == high_i = high_v : xs
        |otherwise = x : aux xs low_i high_i low_v high_v (curr+1)

--Applies the swap function to the given pairs
doSwaps :: [a] -> [(Int, Int)] -> [a]
doSwaps arr [] = arr
doSwaps arr ((i1, i2):xs) = doSwaps (swap arr i1 i2) xs

--Randomly swaps a pair of letters from the strings in the input tuple
swapTup :: (String, String) -> StdGen -> ((String, String), StdGen)
swapTup (s1, s2) gen
    |null s1 || null s2 = ((s1, s2), gen)
    |otherwise = ((out1, out2), gen3)
    where
        (s1_i, gen2) = randomR (0, length s1 - 1) gen
        (s2_i, gen3) = randomR (0, length s2 - 1) gen2
        s1_v = s1 !! s1_i
        s2_v = s2 !! s2_i
        out1 = take s1_i s1 ++ [s2_v] ++ drop (s1_i + 1) s1
        out2 = take s2_i s2 ++ [s1_v] ++ drop (s2_i + 1) s2

--Joins pairs in list of values into tuples
listOfTup :: [a] -> [(a, a)]
listOfTup [] = []
listOfTup[x] = []
listOfTup(x:x':xs) = (x, x') : listOfTup xs

-- Convert a letter to its number representation
alphaToInt :: Char -> Int
alphaToInt c
    |isLower c = ord c - ord 'a'
    |isUpper c = ord c - ord 'A'
    |otherwise = error "alphaToInt -- unexpected character"

--Shift an input character by the specified amount
shiftChar:: Char -> Int -> Char
shiftChar c q 
    |isUpper(c) = chr (ord 'A' + (alphaToInt c + q) `mod` 26)
    |isLower(c) = chr (ord 'a' + (alphaToInt c + q) `mod` 26)

--Remove the value at the given index from the list
del :: [a] -> Int -> [a]
del xs i = take i xs ++ drop (i + 1) xs

-- Samples n_samp unique numbers from [0, x] uniformly, returning a list of samples 
sample :: Int -> [a] -> StdGen -> ([a], StdGen)
sample 0 _ gen = ([], gen)
sample _ [] gen = ([], gen)
sample num_samp arr gen =
  let (samp_ind, gen1) = randomR (0, length arr - 1) gen
      val = arr !! samp_ind
      new_arr = del arr samp_ind
      (rest, gen2) = sample (num_samp - 1) new_arr gen1
  in (val : rest, gen2)

--Transformation Functions

-- Function to implement the wordSwap transformation, which randomly scrambles
-- the word order of the input string.
wordSwap :: String -> [Int] -> String
wordSwap inp samples = out where
    word_lst = words inp
    samp_pairs = listOfTup samples
    out = unwords (doSwaps word_lst samp_pairs)
    
--Randomly Swaps the characters of words in a string
charSwap:: String -> StdGen -> String 
charSwap s gen = out where
    (n_samp, gen2) = randomR(0, (length s - 1) `div` 4) gen --Randomize number of swaps
    samples = fst (sample n_samp [0..length s-1] gen2)
    samp_pairs = listOfTup samples
    out = doSwaps s samp_pairs

--Shifts the tokens in each word by a random, distinct amount
shiftMessage:: String -> [Int] -> String
shiftMessage inp samps = unwords (makeShifts word_lst samps) where
    word_lst = words inp
    doShift :: String -> Int -> String
    doShift [] _ = []
    doShift (x:xs) val
        |isAlpha x = shiftChar x val : doShift xs val
        |otherwise = x : doShift xs val
    
    makeShifts :: [String] -> [Int] -> [String]
    makeShifts [] _ = []
    makeShifts (x:xs) (n:ns) = doShift x n : makeShifts xs ns


--Merges adjacent words at the given indices
mergeWords :: String -> [Int] -> String
mergeWords inp samples = unwords (merge word_lst samples 0) where
    word_lst = words inp
    merge:: [String] -> [Int] -> Int -> [String]
    merge [] _ _ = []
    merge xs [] _ = xs 
    merge (x:x':xs) (i:is) curr
        |curr == i = (x ++ x') : merge xs is (curr+2)
        |otherwise = x : merge (x':xs) (i:is) (curr+1)
    merge (x:xs) _ _ = (x:xs)

-- Function the determines the order of transformations then applies those transformations.
-- Returns a tuple of (list of sequentially transformed inputs, order of applied transformations)
doTransformations:: String -> StdGen -> ([String], [Int])
doTransformations inp gen =  out where 
    word_lst = words inp
    (t_order, gen2) = sample 4 [1..4] gen --Randomize order of transformations
    (n_word_swaps, gen3) = randomR(2, 10) gen2
    (n_merges, gen4) = randomR(0, 3) gen3
    (word_samps, gen5) = sample n_word_swaps [0..length word_lst -1] gen4
    (m_s, gen6) = sample n_merges [0..length word_lst -1] gen5
    merge_samps = (word_samps !! 0) : m_s
    (shift_samps, gen7) = sample 21 [0..25] gen6

    makeTransforms :: String -> [Int] -> ([Int], [Int], [Int]) -> StdGen -> [String]
    makeTransforms _ [] _ _ = []
    makeTransforms s (x:xs) (w_samp, m_samp, shift_samps) gen =
        let out =
                case x of
                1 -> shiftMessage s shift_samps
                2 -> charSwap s gen
                3 -> wordSwap s w_samp
                _ -> mergeWords s m_samp
        in out : makeTransforms out xs (w_samp, m_samp, shift_samps) gen
    transfs = makeTransforms inp t_order (word_samps, merge_samps, shift_samps) gen6
    out = (transfs, t_order)


printStrLst :: [String] -> IO()
printStrLst [] = return ()
printStrLst (x:xs) = do 
    putStrLn "===TRANSFORMATION===="
    putStrLn x
    printStrLst xs

printTransforms:: ([String], [Int]) -> IO()
printTransforms (t_strs, t_order)= do
    putStrLn "===== ORDER ===== \n"
    print t_order
    putStrLn "===== Products ===== \n"
    printStrLst t_strs

--Game logic and I/O s

--Prints the text for a given round
printRound :: String -> String -> Int -> Int -> IO()
printRound original transformed score rounds= do
    putStrLn "============== Original Text =============="
    putStrLn (wrapWords 60 original ++ "\n")
    putStrLn "============= Transformed Text ============="
    putStrLn (wrapWords 60 transformed ++ "\n")
    putStrLn ("Score: " ++ (show score) ++ "Remaining Rounds: " ++ (show rounds)) 

endGame :: Int -> IO()
endGame score = putStrLn ("Final Score: " ++ (show score) ++ "\n\n")

gameLoop :: String -> [String] -> [Int] -> Int -> Bool -> IO ()
gameLoop _ [] _ score _ = do
    endGame score
    exitSuccess

gameLoop original (s:ss) (t:ts) score print_check = do
    if print_check
        then printRound original s score (length (t:ts) - 1)
        else pure ()

    input <- getLine 
    case input of
        "1" ->
            if t == 1
                then gameLoop original ss ts (score + 1) True
                else gameLoop original ss ts score True

        "2" ->
            if t == 2
                then gameLoop original ss ts (score + 1) True
                else gameLoop original ss ts score True

        "3" ->
            if t == 3
                then gameLoop original ss ts (score + 1) True
                else gameLoop original ss ts score True

        "4" ->
            if t == 4
                then gameLoop original ss ts (score + 1) True
                else gameLoop original ss ts score True

        "transformations" -> do
            printTransformationRules
            gameLoop original (s:ss) (t:ts) score False

        "rules" -> do
            printGameRules
            gameLoop original (s:ss) (t:ts) score False

        "render" -> do
            printRound original s score (length (t:ts) - 1)
            gameLoop original (s:ss) (t:ts) score False
        
        "quit" -> exitSuccess

        _ -> do
            printInputError
            gameLoop original (s:ss) (t:ts) score print_check
            

main :: IO ()
main = do
    gen <- newStdGen
    fileText <- readFile "words_clean.txt"
    printGameRules
    putStrLn "\nPress Enter to Start! \n"
    input <- getLine
    let clean = loadText fileText gen
    let (transformations, order) = doTransformations clean gen
    gameLoop clean (reverse transformations) (reverse order) 0 True

    

-- main :: IO()
-- main2 = do
--     gen <- newStdGen
--     game_text <- loadText gen

    -- putStrLn (loadText fileText gen)
    -- putStrLn ("You said:" ++ input)
    -- printRound "There once was a story " "There once was another story "
    -- print (swap [0,1,2,3,4] 0 3)
    -- print (del [0,1,2,3,4] 0)
    -- --swaps <- (listOfTup (fst (sample 6 [1..10] gen)))
    -- print (fst (sample 6 [1..6] gen))
    -- print (doSwaps [0..10] (listOfTup (fst (sample 6 [1..10] gen))))
    -- putStrLn (charSwap "There once was a story " gen)
    -- putStrLn (wordSwap "There once was a story " [0,2])
    -- putStrLn (shiftMessage "We was there" [1,2,3,4])
    -- putStrLn(mergeWords "Zero One Two Three Four Five Six" [0,6])
    -- printTransforms (doTransformations"Zero One Two Three Four Five Six Sev Eigh Nin Ten Elev Twel Thir Fourt Fift Sixt Sevt Eightn Nintn Twent" gen) 
    -- main