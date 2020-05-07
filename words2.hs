-- Nathan Ormond 16500781
import Data.Char
import Data.Function (on)
import Data.List (sortBy)
import Data.Typeable

-- Text input
text = "It was the best of times, it was the worst of times, it was the age of wisdom, it was \
\the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the \
\season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of \
\despair, we had everything before us, we had nothing before us, we were all going direct to \
\Heaven, we were all going direct the other way--in short, the period was so far like the \
\present period, that some of its noisiest authorities insisted on its being received, for good \
\or for evil, in the superlative degree of comparison only.\nThere were a king with a large \
\jaw and a queen with a plain face, on the throne of England; there were a king with a large \
\jaw and a queen with a fair face, on the throne of France. In both countries it was clearer \
\than crystal to the lords of the State preserves of loaves and fishes, that things in general \
\were settled for ever."

-- Returns an array of the top twenty most frequently occuring words in English
twentyFreqWords = 
	wordsWhen (==' ') "the be to of and a in that have i it for not on with he as you do at"

--toWordList : 		Takes a string, lowercases it, 
--					drops any character that is not 
-- 					a letter or a space, and returns 
--					a list with the words in the string.
toWordList :: String -> [String]
toWordList str = 
	wordsWhen (==' ') (removePunc (replace '\n' ' '  (toLowerString str) ))


replace :: Eq a => a -> a -> [a] -> [a]
replace a b = 
	map $ \c -> if c == a then b else c

--countCommonWords: Takes a list of strings and returns the 
--					number of times the 20 most frequently used 
--					English words appears in the list.
countCommonWords :: [String] -> Int
countCommonWords lst = 
	sum [numInList item lst | item <- twentyFreqWords]
	
-- Returns the number of occurences of an element in a list
numInList :: Ord a => a -> [a] -> Int
numInList x xs = 
	(length . filter (== x)) xs

--dropCommonWords: 	Takes a list of strings and drops 
--					any word that is within the top 
--					20 most commonly used in 
--					English. Returns a list of 
--					strings without those words.
dropCommonWords :: [String] -> [String]
dropCommonWords lst = 
	filter (\x -> not (x `elem` twentyFreqWords)) lst

-- Removes all duplicate items from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- Removes a single String as it occurs in a set of Strings
dropSingleWord :: [String] -> String -> [String]
dropSingleWord lst wrd = 
	[x | x <- lst, x/= wrd]

--countWords :		Takes a list of strings and 
--					returns a list. Each element of 
--					the returned list is a tuple which 
--					contains a string (a word) and an integer 
--					(representing the number of times the 
--					word appears in the text).
countWords :: [String] -> [(String, Int)]
countWords lst = 
	[getWordCountTuple lst item | item <- (removeDuplicates lst)]

-- Returns a tuple of the word count of item in a lst
getWordCountTuple :: [String] -> String -> (String, Int)
getWordCountTuple lst item = 
	(item , numInList item lst)



-- Removes punctuation from a string
removePunc :: String -> String
removePunc xs = 
	[ x | x <- xs, not (x `elem` ",.?!()-:;\"\'") ]

-- Makes a String (an array of Char) lower case
toLowerString :: String -> String
toLowerString str = 
	[ toLower loweredString | loweredString <- str]

-- Split string by delimeter
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

numberToStars :: Int -> String
numberToStars size = 
	flip replicate '*' size 

tupleNumToStarsString :: [(String, Int)] -> [(String,String)]
tupleNumToStarsString lst =
	[(word , numberToStars barchart) | (word, barchart) <- lst]

sortWords :: Ord b => [(a, b)] -> [(a, b)]
sortWords = sortBy (flip compare `on` snd)

reverseTuple :: [(String, String)] -> [(String, String)]
reverseTuple lst = 
	[(y,x) | (x,y) <- lst]

makeHistogram :: [(String,Int)] -> String
makeHistogram lst = 
	unlines [ x ++ "->" ++  ys ++ "\n" | (x, ys) <- (reverseTuple $ tupleNumToStarsString lst) ]
--unlines [ x ++ "->" ++ unwords (map show ys) | (x, ys) <- lst ]

main = do
	let wordlist = toWordList text
	putStrLn "Report:"
	putStrLn ("\t" ++ (show $ length wordlist) ++ " words")
	putStrLn ("\t" ++ (show $ countCommonWords wordlist) ++ " common words")
	putStrLn "\nHistogram of the most frequent words (excluding common words):\n"
	putStr $ makeHistogram $ sortWords $ countWords $ dropCommonWords $ wordlist

--TESTING
--	print $ "TEST: toWordList"
--	print $ toWordList "Hello, World! HELLO!! :-)"
--	print $ "TEST: numInList"
--	print $ numInList "the" ["the","planet","of","the","apes"]
--	print $ "TEST: countCommonWords"
--	print $ countCommonWords ["the","planet","of","the","apes"]
--	print $ dropSingleWord ["the","planet","of","the","apes"] "apes"
--	print $ "TEST: dropCommonWords"
--	print $ dropCommonWords ["the","planet","of","the","apes"]
--	print $ "TEST: getWordCountTuple"
--	print $ getWordCountTuple ["the","planet","of","the","apes"] "the"
--	print $ "TEST: removeDuplicates"
--	print $ removeDuplicates ["the","planet","of","the","apes"]
--	print $ "TEST: countWords"
--	print $ countWords ["friend","she","she"]
--	print $ "-----------------------------"
--	print $ tupleNumToStarsString [("four", 4), ("five", 5), ("two", 2), ("one",1), ("three",3)]
--	print $ "-----------------------------"
--	print $ reverseTuple $ tupleNumToStarsString [("four", 4), ("five", 5), ("two", 2), ("one",1), ("three",3)]
--	print $ "-----------------------------"
--	putStr $ makeHistogram [("four", 4), ("five", 5), ("two", 2), ("one",1), ("three",3)]
--	print $ "-----------------------------"
--	putStrLn $ makeHistogram $ sortWords [("four", 4), ("five", 5), ("two", 2), ("one",1), ("three",3)]
