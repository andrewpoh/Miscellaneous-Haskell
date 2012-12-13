{-# OPTIONS_GHC -funbox-strict-fields #-}
import Control.Monad
import System
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap.Strict as M
import qualified Data.List as L
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T

data FieldCount = FieldCount !Bool !Bool !Bool !Int (M.IntMap Int) !Int

main :: IO ()
main = do
	args <- System.getArgs
	let fileName = head args
	countFile fileName

instance Show FieldCount where
	show fieldCount =
		let (histogram, rows) = finaliseFieldCount fieldCount in
		(show rows) ++ " lines: " ++ (show $ M.toList histogram)

countFile :: String -> IO()
countFile filename = do
	byteString <- B.readFile filename
	let fc = countByteString byteString
	print fc

finaliseFieldCount :: FieldCount -> (M.IntMap Int, Int)
finaliseFieldCount (FieldCount infield0 _ _ separators0 counts0 rows0) =
	let additionalField = if infield0 then 1 else 0 in
	let finalFields = separators0+additionalField in
	let finalLine = finalFields > 0 in
	let finalMap = if finalLine
		then (M.insertWith (+) finalFields 1 counts0)
		else counts0
	in
	(finalMap, if finalLine then rows0 + 1 else rows0)

countByteString :: B.ByteString -> FieldCount
countByteString contents =
	let contentsText = E.decodeUtf8 contents in
	let result = T.foldl' countColumnsRows initialFieldCount contentsText in
	result

initialFieldCount :: FieldCount
initialFieldCount =
	FieldCount False False False 0 M.empty 0

countColumnsRows :: FieldCount -> Char -> FieldCount
countColumnsRows fc@(FieldCount infield0 inquotes0 doublequote0 separators0 counts0 rows0) char =
	if char == '\r' then fc -- Discard line feeds! This doesn't work for environments where \r by itself is used as newline
	else
	let plusOne = (M.insertWith (+) (separators0+1) 1 counts0) in
	if inquotes0 then
		if not doublequote0 then -- Within quotes with no preceding quote
			if char=='\"' then FieldCount infield0 True True separators0 counts0 rows0 else fc
		else -- Within quotes with preceding quote, determine whether terminates or is escaped quote
			if char=='\"' then FieldCount infield0 True False separators0 counts0 rows0 -- escaped quote
			else if char == '\n' then
				seq plusOne $ FieldCount False False False 0 plusOne (rows0+1) -- next line
			else if char == ',' then FieldCount False False False (separators0+1) counts0 rows0 -- next field
			else FieldCount infield0 False False separators0 counts0 rows0 -- shouldn't happen, bad field
	else
		if char == '\n' then
			seq plusOne $ FieldCount False False False 0 plusOne (rows0+1) -- next line
		else if char == ',' then FieldCount False False False (separators0+1) counts0 rows0 -- next field
		else if char == '\"' then FieldCount True True False separators0 counts0 rows0 -- quoted field
		else FieldCount True inquotes0 doublequote0 separators0 counts0 rows0

check :: String -> IO ()
check t =
	let runCount = L.foldl' countColumnsRows initialFieldCount in
	putStrLn ("\"" ++ t ++ "\" - " ++ show (runCount t))

ex0 = "a,b,c"
ex1 = "a,b,c\n"
ex2 = "a,b,c\nd,e,f\n"
ex3 = "a,b,c\nd,e,f"
ex4 = "a,\"b,c\"\n"
ex5 = "a,\"b,\"\"c\",d\n"
ex6 = "a,b\n\"c,d\""
