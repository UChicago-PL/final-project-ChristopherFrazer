module Main where

import Data.Char (isLower, isUpper, ord, chr, isAlpha)
import System.Random (randomR, StdGen, getStdGen, newStdGen)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

-- ---------------------------------------------------------------------------
-- Color-Coding Functionality
-- ---------------------------------------------------------------------------

data Color = NoColor | GreenColor | BlueColor | RedColor deriving (Eq)

type ColoredText = [(Char, Color)]

colorPriority :: Color -> Int
colorPriority NoColor = 0
colorPriority GreenColor = 1
colorPriority BlueColor = 2
colorPriority RedColor = 3

-- Keep whichever color has the higher priority
mergeColor :: Color -> Color -> Color
mergeColor c1 c2
    | colorPriority c2 > colorPriority c1 = c2
    | otherwise = c1

-- Apply a color to every character, enforcing color priority
setColor :: Color -> ColoredText -> ColoredText
setColor c = map (\(ch, existing) -> (ch, mergeColor existing c))

fromString :: String -> ColoredText
fromString = map (\c -> (c, NoColor))

toString :: ColoredText -> String
toString = map fst

-- Split ColoredText into words 
coloredWords :: ColoredText -> [ColoredText]
coloredWords [] = []
coloredWords ct =
    let nonSpace = dropWhile (\(c, _) -> c == ' ') ct
    in if null nonSpace then []
       else let (w, rest) = break (\(c, _) -> c == ' ') nonSpace
            in w : coloredWords rest

-- Join colored words with plain spaces
coloredUnwords :: [ColoredText] -> ColoredText
coloredUnwords [] = []
coloredUnwords [w] = w
coloredUnwords (w:ws) = w ++ [(' ', NoColor)] ++ coloredUnwords ws

-- Render ColoredText to a String with terminal color codes
renderColored :: ColoredText -> String
renderColored ct = go ct NoColor
  where
    go [] cur = if cur /= NoColor then "\ESC[0m" else ""
    go ((c, color):rest) cur
        | color == cur = c : go rest cur
        | color == NoColor = "\ESC[0m" ++ (c : go rest NoColor)
        | cur == NoColor = colorCode color ++ (c : go rest color)
        | otherwise = "\ESC[0m" ++ colorCode color ++ (c : go rest color)
    colorCode GreenColor = "\ESC[32m"
    colorCode BlueColor = "\ESC[34m"
    colorCode RedColor = "\ESC[31m"
    colorCode NoColor = ""

-- Word-wrap ColoredText at the given column width (width measured in characters,
-- not escape codes, so the count is correct)
wrapColoredWords :: Int -> ColoredText -> ColoredText
wrapColoredWords width ct = go (coloredWords ct) []
  where
    go [] [] = []
    go [] currentLine = currentLine
    go (w:ws) [] = go ws w
    go (w:ws) currentLine
        | length currentLine + 1 + length w <= width =
            go ws (currentLine ++ [(' ', NoColor)] ++ w)
        | otherwise =
            currentLine ++ [('\n', NoColor)] ++ go ws w

-- ---------------------------------------------------------------------------
-- Functions to Print Static Messages
-- ---------------------------------------------------------------------------

printGameRules :: IO()
printGameRules = do
    text <- readFile "rules.txt"
    mapM_ putStrLn (lines text)

printInputError :: IO()
printInputError = do
    text <- readFile "inputs.txt"
    mapM_ putStrLn (lines text)

printTransformationRules :: IO()
printTransformationRules = do
    text <- readFile "transformations.txt"
    mapM_ putStrLn (lines text)

printGuesses :: IO()
printGuesses = do
    putStrLn "Guess: 1 - Shift, 2 - Character Swap, 3 - Word Swap, 4 - Word Merge"

-- ---------------------------------------------------------------------------
-- General Helper Functions
-- ---------------------------------------------------------------------------

-- Extracts a 20-word excerpt from the input text
loadText :: String -> StdGen -> String
loadText text gen = out where
    word_lst = words text
    (excerpt_ind, gen2) = randomR (0, length word_lst-21) gen
    words_20 = take 20 (drop excerpt_ind word_lst)
    out = unwords words_20

-- Wrap text to the specified character length by adding newlines
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

-- Swaps the elements at the specified indices in the array
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

-- Applies the swap function to the given pairs
doSwaps :: [a] -> [(Int, Int)] -> [a]
doSwaps arr [] = arr
doSwaps arr ((i1, i2):xs) = doSwaps (swap arr i1 i2) xs

-- Randomly swaps a pair of letters from the strings in the input tuple
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

-- Joins pairs in list of values into tuples
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

-- Shift an input character by the specified amount
shiftChar:: Char -> Int -> Char
shiftChar c q 
    |isUpper(c) = chr (ord 'A' + (alphaToInt c + q) `mod` 26)
    |isLower(c) = chr (ord 'a' + (alphaToInt c + q) `mod` 26)

-- Remove the value at the given index from the list
del :: [a] -> Int -> [a]
del xs i = take i xs ++ drop (i + 1) xs

-- Samples n_samp unique values from arr uniformly, returning a list of samples
sample :: Int -> [a] -> StdGen -> ([a], StdGen)
sample 0 _ gen = ([], gen)
sample _ [] gen = ([], gen)
sample num_samp arr gen =
  let (samp_ind, gen1) = randomR (0, length arr - 1) gen
      val = arr !! samp_ind
      new_arr = del arr samp_ind
      (rest, gen2) = sample (num_samp - 1) new_arr gen1
  in (val : rest, gen2)

-- ---------------------------------------------------------------------------
-- Color-coding Transformation Functions
-- ---------------------------------------------------------------------------

-- wordSwap: swaps word positions, marks moved words GREEN
wordSwapColored :: ColoredText -> [Int] -> ColoredText
wordSwapColored inp samples = coloredUnwords result where
    word_lst = coloredWords inp
    samp_pairs = listOfTup samples
    swapped_idxs = concatMap (\(i, j) -> [i, j]) samp_pairs
    swapped = doSwaps word_lst samp_pairs
    result = [ if i `elem` swapped_idxs then setColor GreenColor w else w
                    | (i, w) <- zip [0..] swapped ]

-- mergeWords: merges adjacent words, marks merged characters BLUE
mergeWordsColored :: ColoredText -> [Int] -> ColoredText
mergeWordsColored inp samples = coloredUnwords (merge word_lst samples 0) where
    word_lst = coloredWords inp
    merge :: [ColoredText] -> [Int] -> Int -> [ColoredText]
    merge [] _ _ = []
    merge xs [] _ = xs
    merge (x:x':xs) (i:is) curr
        | curr == i = setColor BlueColor (x ++ x') : merge xs is (curr+2)
        | otherwise = x : merge (x':xs) (i:is) (curr+1)
    merge (x:xs) _ _ = (x:xs)

-- charSwap: swaps individual characters, marks swapped characters RED
-- Only non-space characters are candidates for swapping (preserves word count/length)
charSwapColored :: ColoredText -> StdGen -> ColoredText
charSwapColored ct gen = out where
    non_space_idxs = [i | (i, (c, _)) <- zip [0..] ct, c /= ' ']
    (n_samp, gen2) = randomR (0, (length ct - 1) `div` 4) gen
    samples = fst (sample n_samp non_space_idxs gen2)
    samp_pairs = listOfTup samples
    swapped = doSwaps ct samp_pairs
    swapped_idxs = concatMap (\(i, j) -> [i, j]) samp_pairs
    out = [(c, if i `elem` swapped_idxs then mergeColor color RedColor else color)
          | (i, (c, color)) <- zip [0..] swapped]

-- shiftMessage: shifts letter values, no color annotation (not color-coded)
shiftMessageColored :: ColoredText -> [Int] -> ColoredText
shiftMessageColored inp samps = coloredUnwords (makeShifts word_lst samps) where
    word_lst = coloredWords inp
    doShift :: ColoredText -> Int -> ColoredText
    doShift [] _ = []
    doShift ((x, color):xs) val
        | isAlpha x = (shiftChar x val, color) : doShift xs val
        | otherwise = (x, color) : doShift xs val
    makeShifts :: [ColoredText] -> [Int] -> [ColoredText]
    makeShifts [] _ = []
    makeShifts xs [] = xs
    makeShifts (x:xs) (n:ns) = doShift x n : makeShifts xs ns

-- ---------------------------------------------------------------------------
-- Applying transformations
-- ---------------------------------------------------------------------------

-- Determines transformation order then applies them, returning color-annotated
-- intermediate results alongside the transformation order.
doTransformations :: String -> StdGen -> ([ColoredText], [Int])
doTransformations inp gen = out where
    word_lst = words inp
    (t_order, gen2) = sample 4 [1..4] gen
    (n_word_swaps, gen3) = randomR(2, 4) gen2
    (n_merges, gen4) = randomR(1, 3) gen3
    (word_samps, gen5) = sample n_word_swaps [0..length word_lst - 1] gen4
    (m_s, gen6) = sample n_merges [0..length word_lst - 1] gen5
    merge_samps = (word_samps !! 0) : m_s
    (shift_samps, _)   = sample 21 [0..25] gen6

    makeTransforms :: ColoredText -> [Int] -> ([Int], [Int], [Int]) -> StdGen -> [ColoredText]
    makeTransforms _ [] _ _ = []
    makeTransforms s (x:xs) (w_samp, m_samp, s_samps) g =
        let result = case x of
                1 -> shiftMessageColored s s_samps
                2 -> charSwapColored s g
                3 -> wordSwapColored s w_samp
                _ -> mergeWordsColored s m_samp
        in result : makeTransforms result xs (w_samp, m_samp, s_samps) g

    colored_inp = fromString inp
    transfs = makeTransforms colored_inp t_order (word_samps, merge_samps, shift_samps) gen6
    out = (transfs, t_order)

-- ---------------------------------------------------------------------------
-- Display helpers
-- ---------------------------------------------------------------------------

printStrLst :: [ColoredText] -> IO()
printStrLst [] = return ()
printStrLst (x:xs) = do
    putStrLn "===TRANSFORMATION===="
    putStrLn (renderColored x)
    printStrLst xs

printTransforms :: ([ColoredText], [Int]) -> IO()
printTransforms (t_strs, t_order) = do
    putStrLn "===== ORDER ===== \n"
    print t_order
    putStrLn "===== Products ===== \n"
    printStrLst t_strs

-- ---------------------------------------------------------------------------
-- Game logic and I/O
-- ---------------------------------------------------------------------------

-- Builds two per-game hints based on the  transformation order.
-- Transformation codes: 1=shiftMessage, 2=charSwap, 3=wordSwap, 4=mergeWords
generateHints :: [Int] -> String
generateHints t_order = "Hints:\n  1. " ++ hint1 ++ "\n  2. " ++ hint2
  where
    pos t = length (takeWhile (/= t) t_order)
    hint1 = if pos 2 < pos 3
                then "Characters swapped before words swapped"
                else "Words swapped before characters swapped"
    hint2 = if pos 4 < pos 2
                then "Words merged before characters swapped"
                else "Characters swapped before words merged"

printRound :: String -> ColoredText -> String -> Int -> Int -> IO()
printRound original transformed hints score rounds = do
    putStrLn "============== Original Text =============="
    putStrLn (wrapWords 60 original ++ "\n")
    putStrLn "============= Transformed Text ============="
    putStrLn (renderColored (wrapColoredWords 60 transformed) ++ "\n")
    printGuesses
    putStrLn ("\n" ++ hints ++ "\n")
    putStrLn ("Score: " ++ show score ++ "   Remaining Rounds: " ++ show rounds)

endGame :: Int -> IO()
endGame score = putStrLn ("Final Score: " ++ (show score) ++ "\n\n")

gameLoop :: String -> [ColoredText] -> [Int] -> String -> Int -> Bool -> IO ()
gameLoop _ [] _ _ score _ = do
    endGame score
    exitSuccess

gameLoop original (s:ss) (t:ts) hints score print_check = do
    if print_check
        then printRound original s hints score (length (t:ts) - 1)
        else pure ()

    input <- getLine 
    case input of
        "1" ->
            if t == 1
                then gameLoop original ss ts hints (score + 1) True
                else gameLoop original ss ts hints score True

        "2" ->
            if t == 2
                then gameLoop original ss ts hints (score + 1) True
                else gameLoop original ss ts hints score True

        "3" ->
            if t == 3
                then gameLoop original ss ts hints (score + 1) True
                else gameLoop original ss ts hints score True

        "4" ->
            if t == 4
                then gameLoop original ss ts hints (score + 1) True
                else gameLoop original ss ts hints score True

        "transformations" -> do
            printTransformationRules
            gameLoop original (s:ss) (t:ts) hints score False

        "rules" -> do
            printGameRules
            gameLoop original (s:ss) (t:ts) hints score False

        "render" -> do
            printRound original s hints score (length (t:ts) - 1)
            gameLoop original (s:ss) (t:ts) hints score False
        
        "quit" -> exitSuccess

        _ -> do
            printInputError
            gameLoop original (s:ss) (t:ts) hints score print_check

main :: IO ()
main = do
    gen <- newStdGen
    fileText <- readFile "words_clean.txt"
    printGameRules
    putStrLn "\nPress Enter to Start! \n"
    input <- getLine
    let clean = loadText fileText gen
    let (transformations, order) = doTransformations clean gen
    let hints = generateHints order
    gameLoop clean (reverse transformations) (reverse order) hints 0 True