module Huffman where

import Data.Map (fromListWith)
import Data.Map (toList)
import Data.List (sortBy, insertBy)

-- Use this to test! First apply readInput in REPL. Then write the input string and press enter.
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

-- Takes a msg with an encodingTable and returns the encoded msg
encodeMessage :: String -> [([Char], Char)] -> String
encodeMessage [] _ = [] -- If message is empty
encodeMessage (char:rest) encodingTable = let encodingOfChar = lookupByValue encodingTable char -- Path as "110"
                                          in
                                              case encodingOfChar of
                                                Nothing -> "Could not encode message"
                                                Just encodedChar -> 
                                                  encodedChar ++ encodeMessage rest encodingTable -- Add the rest of the encodings

-- Takes an encoded msg and an encodingTable and returns the encoded msg
decodeMessage :: String -> [([Char], Char)] -> Maybe String
decodeMessage [] _ = Just []
decodeMessage msg encodingTable = decodeMessageHelper msg encodingTable 1 ""

-- A helper function for the decodeMessage function, that includes an i, that is the current decoding position.
-- For example if i = 2 and msg = "10001", we check the first 2, i.e. "10".
-- Additionally an accumulator for the decoded msg is added, i.e. the decodedMsg.
decodeMessageHelper :: String -> [([Char], Char)] -> Int -> String -> Maybe String
decodeMessageHelper [] encodingTable i [] = Nothing -- If nothing decoded, and nothing left in string -> Nothing
decodeMessageHelper [] encodingTable i decodedMsg = Just decodedMsg -- If something decoded, and the rest of the msg is empty
decodeMessageHelper msg encodingTable i decodedMsg = let org_text = lookup (take i msg) encodingTable
                                                     -- See if the current path (e.g. "100") yields a char
                                                     in
                                                        case org_text of
                                                          -- If not, include the next int from the path (1 or 0)
                                                          Nothing -> decodeMessageHelper msg encodingTable (i + 1) decodedMsg
                                                          -- Else drop the used 0s and 1s and try to decode the rest.
                                                          -- Also add the decoded char to the decodedMsg accumulator
                                                          Just decodedChar ->
                                                              decodeMessageHelper (drop i msg) encodingTable 1 (decodedMsg ++ [decodedChar])












--------------------------------------
-------- HUFFMAN TREE STUFF ----------
--------------------------------------

data HTree  = Leaf Int Char
            | Node Int [Char] HTree HTree
            deriving (Show)

-- For merging two trees, while summing up their value and strings
mergeTrees :: HTree -> HTree -> HTree
mergeTrees t1 t2  = Node (nodeValue t1 + nodeValue t2) (nodeChars t1 ++ nodeChars t2) t1 t2

-- Get int value of a node
nodeValue :: HTree -> Int
nodeValue (Leaf v _) = v
nodeValue (Node v _ _ _) = v

-- Get chars from a node
nodeChars :: HTree -> [Char]
nodeChars (Leaf _ c) = [c]
nodeChars (Node _ cs _ _) = cs

createHuffManTree :: String -> HTree
createHuffManTree string = let sortedFreqs = sortFrequencies (frequencies string) -- Char frequencies from the string input
                               leafNodes = map (\x -> Leaf (snd x) (fst x)) sortedFreqs -- Create all leafs
                           in
                              huffManTreeHelper leafNodes -- Use the helper to build the tree from the leaf nodes

huffManTreeHelper :: [HTree] -> HTree
huffManTreeHelper [] = Leaf 0 ' ' -- Dummy. If empty string, simply create a single empty node
huffManTreeHelper [t1] = t1 -- If the list only has one element, return that element. This is the root.
huffManTreeHelper (first:second:rest) = -- Take two first, since they are sorted, these always have the lowest value and should be used next
                                        let n = Node
                                                ((nodeValue first) + (nodeValue second))
                                                ((nodeChars first) ++ (nodeChars second))
                                                first
                                                second
                                          in
                                             -- Add new node to the list, sort it again and rerun with the new list
                                             huffManTreeHelper (sortHuffManTreeByValue (n : rest))

-- Sorts a list of Huffman trees by the node value in increasing order
sortHuffManTreeByValue :: [HTree] -> [HTree]
sortHuffManTreeByValue [] = []
sortHuffManTreeByValue [t] = [t]
sortHuffManTreeByValue lst = sortBy (\t1 t2 -> compare (nodeValue t1) (nodeValue t2)) lst

-- Create the map from char to how many occurrences of that char in the string
-- FromListWith takes a list of pairs and function to apply, if some key is present more than once
frequencies :: String -> [(Char, Int)]
frequencies s = toList (fromListWith (+) [(c, 1) | c <- s])

-- Sort the frequency map by most occurrences
sortFrequencies :: [(Char, Int)] -> [(Char, Int)]
sortFrequencies = sortBy (\ a b -> compare (snd b) (snd a))

-- Result tree for mississippi river as example
resultTree :: HTree
resultTree = Node 17 "prmv eis" (Node 8 "prmv e"
                                          (Node 4 "pr" (Leaf 2 'p')
                                                       (Leaf 2 'r'))
                                          (Node 4 "mv e"
                                                        (Node 2 "mv" (Leaf 1 'm')
                                                                     (Leaf 1 'v'))
                                                        (Node 2 " e" (Leaf 1 ' ')
                                                                     (Leaf 1 'e'))))
                                (Node 9 "is" (Leaf 5 'i')
                                             (Leaf 4 's'))

--------------------------------------
------ HUFFMAN TREE STUFF END --------
--------------------------------------









--------------------------------------
------ ENCODING/DECODING TABLE -------
--------------------------------------

-- Make encoding / decoding table, which is a map from a binary path (e.g. "00") to a char
makeTable :: HTree -> [([Char], Char)]
makeTable (Leaf int char) = [("1", char)] -- If we only have a leaf the path is always just 1
makeTable (Node int cs t1 t2) = makeTableHelper t1 "0" ++ makeTableHelper t2 "1"

-- Helper function for make table helper, which includes an accumulator for the path
makeTableHelper :: HTree -> [Char] -> [([Char], Char)]
makeTableHelper (Leaf int char) path = [(path, char)]
makeTableHelper (Node int cs t1 t2) path = makeTableHelper t1 (path ++ "0") ++ makeTableHelper t2 (path ++ "1")

-- Lookup a in a map using a value. Returns the first key, that has the looked up value.
-- This works, since a huffman tree has a unique path for each char, i.e. the same key cannot appear with 
-- two different values.
lookupByValue :: Eq v => [(k, v)] -> v -> Maybe k
lookupByValue [] _ = Nothing
lookupByValue ((key, value) : xs) lookupValue | value == lookupValue = Just key
                                              | otherwise = lookupByValue xs lookupValue

------------------------------------------
------ ENCODING/DECODING TABLE END -------
------------------------------------------
