import Data.Char

--	putStrLn $ show $ reverseWords myString
----------------------------------------
--Function Priority Demo
--a b c d =
--  "Function a called with arguments " ++ b ++ " " ++ c ++ " " ++ d

--b = "b"
--c = "c"
--d = "d"

--main = putStrLn $
-- show (program fragment, to see the rest, press the "Show All" button)
--    a b c d 
-- /show
--------------------------------------

-- Makes a String (an array of Char) lower case
toLowerString :: String -> String
toLowerString str = 
	[ toLower loweredString | loweredString <- str]

-- Joins an array of Strings together, separated by spaces
joinWithSpaces :: [String] -> String
joinWithSpaces lst = 
	drop (length " ") $ concat $ map (\w -> " " ++ w) lst

-- Splits the input string into separate words, returns array
reverseWords :: String -> String
reverseWords str = 
	joinWithSpaces $ reverse $ words $ toLowerString str

getTriple :: Int -> Int
getTriple num = 
	num * 3

getTriples lst = 
	map getTriple lst

numberToXs :: Int -> String
numberToXs size = 
	flip replicate 'x' size ++ "\n"

listToXs :: [Int] -> [String]
listToXs lst = 
	map numberToXs lst

-- Remove duplicates from a list and return 
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- Split string by delimeter
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


uniqueWords :: String -> [String]
uniqueWords input = 
	removeDuplicates $ wordsWhen (==' ') input


vowels = 
	['a', 'e', 'i','o','u']

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (h:t) = 
	if even h then h:allEven t else allEven t

getLetterCount :: Char -> String -> Int
getLetterCount c str = 
	length $ filter (== c) str

getLetterTuple :: Char -> String -> (Char, Int)
getLetterTuple char str = 
	(char, getLetterCount char str)

getVowelCount :: String -> [(Char,Int)]
getVowelCount str =
	[getLetterTuple char str | char <- vowels]

main = do
--	print $ toLowerString "Hello"
--	print $ joinWithSpaces [ "one", "two", "three" ]
--	putStrLn $ show $ reverseWords "This is a test string"
--	print $ getTriples [3,1,2,4]
--	print $ numberToXs 5
--	print $ listToXs $ getTriples [3,1,2,4]
--	print $ show $ uniqueWords "one and two and one and three"
--	putStrLn $ show $ vowels
--	print $ allEven [1,2,3,4,5,6,7,8,9,10]
--	print $ getLetterCount 'a' "many vowels in this sentence"
--	print $ getLetterTuple 'a' "many vowels in this sentence"
	print $ getVowelCount "many vowels in this sentence"




