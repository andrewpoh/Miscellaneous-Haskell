import Control.Monad
import System.Environment
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as T

main :: IO ()
main = do
	args <- getArgs
	if length args < 2 then putStrLn "Usage: grepFile fileName pattern"
	else do
		let fileName = args !! 0
		let pattern = args !! 1
		grepFile fileName pattern

grepFile :: String -> String -> IO ()
grepFile filename pattern = do
	byteString <- B.readFile filename
	let contentsText = E.decodeUtf8 byteString
	let contentsLines = T.lines contentsText
	foldM_ (printMatch (T.pack pattern)) 0 contentsLines

printMatch :: T.Text -> Int -> T.Text -> IO Int
printMatch pattern lineNumber text = do
	when (T.isInfixOf pattern text)
		(putStrLn (show lineNumber ++ ": " ++ T.unpack text))
	return $! (lineNumber+1)
