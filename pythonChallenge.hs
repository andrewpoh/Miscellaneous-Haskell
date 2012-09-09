-- My work on the Python Challenge from 13 May 2005

import Char
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.HashTable as Ht
import qualified Data.Set as Set
import System.IO.Unsafe

-- www.PythonChallenge.com

-- 1
data Letter = Uc Char | Lc Char | Sym Char

instance Show Letter where
	show (Uc c) = [c]
	show (Lc c) = [c]
	show (Sym c) = [c]

succL :: Letter -> Letter
succL (Uc c) = Uc (toEnum nc)
	where
	nc = mod (fromEnum c + 1 - nA) 26 + nA
	nA = fromEnum 'A'
succL (Lc c) = Lc (toEnum nc)
	where
	nc = mod (fromEnum c + 1 - na) 26 + na
	na = fromEnum 'a'
succL c@(Sym _) = c

letter :: Char -> Letter
letter c
	| isUpper c = Uc c
	| isLower c = Lc c
	| otherwise = Sym c

convert :: (Letter -> Letter) -> String -> String
convert f str = foldr ((++) . show) "" (map (f . letter) str)

-- 2

type ChCt = Ht.HashTable Char Int

printHt :: ChCt -> IO ()
printHt ht = Ht.toList ht >>= print

emptyCharCount :: IO ChCt
emptyCharCount = Ht.new (==) (Ht.hashString . return)

calcNewCharCount :: Maybe Int -> Int
calcNewCharCount Nothing = 1
calcNewCharCount (Just n) = n+1

incrCharCount :: ChCt -> Char -> IO ChCt
incrCharCount ht ch = do
	item <- Ht.lookup ht ch
	let newval = calcNewCharCount item
	Ht.update ht ch newval
	return ht

appCharCount :: ChCt -> String -> IO ChCt
appCharCount = foldM incrCharCount 

charCountHt :: String -> IO ChCt
charCountHt str = emptyCharCount >>= \ht -> appCharCount ht str

charCountLst :: String -> [(Char, Int)]
charCountLst str = unsafePerformIO (charCountHt str >>= Ht.toList)

freqChars :: Int -> [(Char, Int)] -> Set.Set Char
freqChars hi lst = Set.mkSet [ch | (ch, ct) <- lst, ct <= hi]

cleanStr :: Set.Set Char -> String -> String
cleanStr st = filter (flip Set.member st)

cleanFreqChars :: Int -> [(Char, Int)] -> String -> String
cleanFreqChars = curry $ cleanStr . uncurry freqChars

doClean :: Int -> String -> String
doClean hi str = cleanFreqChars hi (charCountLst str) str

-- 3

strKeep :: (String -> Maybe Char) -> String -> String
strKeep extract = unfoldr (elKeep extract)

elKeep :: ([a] -> Maybe a) -> [a] -> Maybe (a, [a])
elKeep extract el
	| null el = Nothing
	| isNothing extres = elKeep extract tl
	| otherwise = Just (ext, tl)
	where
	extres = extract el
	Just ext = extres
	tl = tail el

uuuluuu :: String -> Maybe Char
uuuluuu (d1:a1:a2:a3:c:b1:b2:b3:d2:_) =
	if all isLower [d1, d2, c] && all isUpper [a1,a2,a3,b1,b2,b3]
		then Just c
		else Nothing
uuuluuu _ = Nothing
