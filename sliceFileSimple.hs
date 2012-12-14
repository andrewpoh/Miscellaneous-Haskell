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
	let tailLines = drop skipCount (T.lines text) in
	let sliceText = take lineCount tailLines in
	T.concat sliceText
