{-# OPTIONS_GHC -funbox-strict-fields #-}
import Control.Monad
import System
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap.Strict as M
import qualified Data.List as L
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T

data LineData = LineData !Int !Bool !Bool !Bool

main :: IO ()
main = do
	args <- System.getArgs
	if length args < 3 then putStrLn "Usage: filterCsv columnCount inputFile outputFile"
	else do
	let columnCount = read (args !! 0)
	let inputName = args !! 1
	let outputName = args !! 2
	filterFile columnCount inputName outputName

filterFile :: Int -> String -> String -> IO()
filterFile c inputName outputName = do
	byteString <- B.readFile inputName
	let contentsText = E.decodeUtf8 byteString
	let textLines = T.lines contentsText
	let goodLines = L.filter (checkLine c) textLines
	let result = T.unlines goodLines
	B.writeFile outputName (E.encodeUtf8 result)
	putStrLn (show (length goodLines) ++ " lines written")

finaliseLineData :: LineData -> Int
finaliseLineData (LineData count0 infield0 _ _) =
	let additionalField = if infield0 then 1 else 0 in
	let finalCount = count0+additionalField in
	finalCount

checkLine :: Int -> T.Text -> Bool
checkLine count text =
	let lc = finaliseLineData (T.foldl' countFields initialLineData text) in
	count == lc

initialLineData :: LineData
initialLineData =
	LineData 0 False False False

countFields :: LineData -> Char -> LineData
countFields ld@(LineData count0 infield0 inquotes0 doublequote0) char =
	if char == '\r' then ld
	else
	if inquotes0 then
		if not doublequote0 then -- Within quotes with no preceding quote
			if char=='\"' then (LineData count0 infield0 True True)
			else ld
		else -- Within quotes with preceding quote, determine whether terminates or is escaped quote
			if char=='\"' then (LineData count0 infield0 True False) -- escaped quote
			else if char == ',' then (LineData (count0+1) False False False) -- next field
			else ld -- shouldn't happen, bad field
	else
		if char == ',' then LineData (count0+1) False False False -- next field
		else if char == '\"' then LineData count0 True True False -- quoted field
		else LineData count0 True inquotes0 doublequote0

check :: String -> IO ()
check t =
	let runCount = finaliseLineData $ L.foldl' countFields initialLineData t in
	putStrLn ("\"" ++ t ++ "\" - " ++ show (runCount))

ex0 = "a,b,c"
ex1 = "a,b,c\n"
ex2 = "a,\"b,c\"\n"
ex3 = "a,\"b,\"\"c\",d\n"
ex4 = "a,\"b\nc\",d"
