module Huffman where

import Data.Map (fromListWith)
import Data.Map (toList)
import Data.List (sortBy, insertBy)

data HTree  = Leaf Int Char
            | Node Int [Char] HTree HTree
            deriving (Show)

mergeTrees :: HTree -> HTree -> HTree
mergeTrees t1 t2  = Node (nodeValue t1 + nodeValue t2) (nodeChars t1 ++ nodeChars t2) t1 t2

nodeValue :: HTree -> Int
nodeValue (Leaf v _) = v
nodeValue (Node v _ _ _) = v

nodeChars :: HTree -> [Char]
nodeChars (Leaf _ c) = [c]
nodeChars (Node _ cs _ _) = cs

frequencies :: String -> [(Char, Int)]
frequencies s = toList (fromListWith (+) [(c, 1) | c <- s])

sortFrequencies :: [(Char, Int)] -> [(Char, Int)]
sortFrequencies = sortBy (\ a b -> compare (snd b) (snd a))

makeTable :: HTree -> [([Int], Char)]
makeTable (Leaf int char) = [([], char)]
makeTable (Node int cs t1 t2) = makeTableHelper t1 [0] ++ makeTableHelper t2 [1]

makeTableHelper :: HTree -> [Int] -> [([Int], Char)]
makeTableHelper (Leaf int char) pathLst = [(pathLst, char)]
makeTableHelper (Node int cs t1 t2) pathLst = makeTableHelper t1 (pathLst ++ [0]) ++ makeTableHelper t2 (pathLst ++ [1])

myHTree :: HTree
myHTree = Node 1 "abcdefgh" (Node 2 "abc"   (Leaf 3 'a')
                                            (Node 4 "bc"   (Leaf 5 'b') (Leaf 6 'c')))
                            (Node 7 "defgh" (Node 8 "de"   (Leaf 9 'd') (Leaf 10 'e'))
                                            (Node 11 "fgh" (Leaf 12 'f')
                                                           (Node 13 "gh" (Leaf 14 'g') (Leaf 15 'h'))))

readInput :: IO ()
readInput = do
                input <- getLine
                let huffManTree = createHuffManTree input
                    encodingTable = makeTable huffManTree
                    encodedMessage = encodeMessage input encodingTable
                    decodedMessage = decodeMessage encodedMessage encodingTable
                putStrLn $ "You wrote: " ++ input
                putStrLn $ "Huffman Tree: " ++ show huffManTree
                putStrLn $ "Encoding table: " ++ show encodingTable
                putStrLn $ "Encoded message: " ++ encodedMessage
                case decodedMessage of
                  Nothing -> putStrLn "Could not decode"
                  Just decoded -> putStrLn $ "Decoded message: " ++ decoded
                return ()


createHuffManTree :: String -> HTree
createHuffManTree string = let sorted_freqs = sortFrequencies (frequencies string)
                               leaf_nodes = map (\x -> Leaf (snd x) (fst x)) sorted_freqs
                           in
                              huffManTreeHelper leaf_nodes

huffManTreeHelper :: [HTree] -> HTree
huffManTreeHelper [t1] = t1
huffManTreeHelper (first:second:rest) =
                                        let n = Node
                                                ((nodeValue first) + (nodeValue second))
                                                ((nodeChars first) ++ (nodeChars second))
                                                first
                                                second
                                          in
                                             huffManTreeHelper (sortHuffManTreeByValue (n : rest))

sortHuffManTreeByValue :: [HTree] -> [HTree]
sortHuffManTreeByValue [] = []
sortHuffManTreeByValue [t] = [t]
sortHuffManTreeByValue lst = sortBy (\t1 t2 -> compare (nodeValue t1) (nodeValue t2)) lst


-- Result tree for mississippi river
resultTree :: HTree
resultTree = Node 17 "prmv eis" (Node 8 "prmv e"
                                          (Node 4 "pr" (Leaf 2 'p') (Leaf 2 'r'))
                                          (Node 4 "mv e"
                                                        (Node 2 "mv" (Leaf 1 'm') (Leaf 1 'v'))
                                                        (Node 2 " e" (Leaf 1 ' ') (Leaf 1 'e'))))
                                 (Node 9 "is" (Leaf 5 'i') (Leaf 4 's'))

encodeMessage :: String -> [([Int], Char)] -> String
encodeMessage [] _ = []
encodeMessage (char:rest) encodingTable = let pathLstAsInts = lookupByValue encodingTable char -- Path as [1, 1, 0]
                                              pathLstAsStrings = lstOfEleToLstOfString pathLstAsInts -- Path as ["1", "1", "0"]
                                              path = concat pathLstAsStrings -- Path as "110"
                                          in
                                              path ++ encodeMessage rest encodingTable -- Concat all characters


decodeMessage :: String -> [([Int], Char)] -> Maybe String
decodeMessage [] _ = Just []
decodeMessage msg encodingTable = decodeMessageHelper msg encodingTable 1 ""

decodeMessageHelper :: String -> [([Int], Char)] -> Int -> String -> Maybe String
decodeMessageHelper [] encodingTable i [] = Nothing -- If nothing decoded, and nothing left in string -> Nothing
decodeMessageHelper [] encodingTable i decodedMsg = Just decodedMsg
decodeMessageHelper msg encodingTable i decodedMsg = let intList = lstOfStringsToInt (take i msg) -- Encoding as [1, 0, 1, 0]
                                                         org_text = lookup intList encodingTable
                                                     in
                                                        case org_text of
                                                          Nothing -> decodeMessageHelper msg encodingTable (i + 1) decodedMsg
                                                          Just decodedChar ->
                                                              decodeMessageHelper (drop i msg) encodingTable 1 (decodedMsg ++ [decodedChar])

containsKey :: Eq k => k -> [(k, v)] -> Bool
containsKey k assocList = k `elem` [fst x | x <- assocList]

lookupByValue :: Eq v => [(k, v)] -> v -> k
lookupByValue ((key, value) : xs) lookupValue | value == lookupValue = key
                                              | otherwise = lookupByValue xs lookupValue


lstOfEleToLstOfString :: Show a => [a] -> [String]
lstOfEleToLstOfString = (>>= return.show)

lstOfStringsToInt :: String -> [Int]
lstOfStringsToInt str = map (read . pure :: Char -> Int) str

-- Huffman Coding Algorithm
-- ENCODING:
-- Calculate frequencies
-- Sort Frequencies
-- Create huffman tree (both nodes and leafs have a string text and an Int value)
-- Make table of the tree to look up paths, i.e. bit representation of a char
-- For each char look up bit representation and add to list

-- DECODING
-- While list of bits is not empty
  -- Traverse tree with each bit until leaf is found
  -- Add char at the end to new string