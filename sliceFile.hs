-- Slices particular lines from a file and sends them to standard output
-- or a file
-- Written September-October 2012
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Maybe
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T

main :: IO ()
main = do
	args <- getArgs
	progName <- getProgName
	checkUsage progName args
	let skipCount = read (args !! 0)
	let lineCount = read (args !! 1)
	let inputName = args !! 2
	let outputName = if length args < 4 then Nothing else Just (args !! 3)
	sliceFile inputName outputName skipCount lineCount

checkUsage :: String -> [String] -> IO ()
checkUsage progName args =
	if (length args < 3) then
		let messageTail = " skipCount lineCount inputFile [outputFile]" in
		let message = "Usage: "++progName++messageTail in
		putStrLn message >>
		exitFailure
	else
		return ()

sliceFile :: String -> Maybe String -> Int -> Int -> IO ()
sliceFile inputName maybeOutputName skipCount lineCount = do
	inputContents <- liftM E.decodeUtf8 $ B.readFile inputName
	let outputText = sliceText skipCount lineCount inputContents
	let outputContent = E.encodeUtf8 outputText
	maybe (B.putStr outputContent >> putChar '\n')
		(flip B.writeFile outputContent) maybeOutputName

sliceText :: Int -> Int -> T.Text -> T.Text
sliceText skipCount lineCount text =
	let tailLines = nTimes skipCount skipLine text in
	let sliceText = keepLines lineCount tailLines in
	sliceText

skipLine :: T.Text -> T.Text
skipLine = (T.drop 1) . (T.dropWhile ('\n'/=))

keepLines :: Int -> T.Text -> T.Text
keepLines n t = T.intercalate "\n" $ reverse $ fst (nTimes n keepLines_ ([], t))

keepLines_ :: ([T.Text], T.Text) -> ([T.Text], T.Text)
keepLines_ y@(ls, r0) =
	if T.null r0 then
		y
	else
	let (l, r1) = T.span ('\n'/=) r0 in
	let r2 = T.drop 1 r1 in
	(l:ls, r2)

nTimes :: Int -> (a->a) -> a -> a
nTimes n f x = L.foldl' (\a b-> f a) x (L.replicate n ())
