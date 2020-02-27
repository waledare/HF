import qualified Data.ByteString.Char8 as L 
import Data.List
import System.IO
import Control.Exception

file :: String
file = "char.csv"

countLines :: L.ByteString -> Int
countLines = length . L.split '\n' 

countCols :: L.ByteString -> Int
countCols = length . L.split ',' 

dimension :: L.ByteString -> (Int , [Int])
dimension xs = 
    let lines = L.split '\n' xs 
        lineCount = length lines
        rowDim = nub $ map countCols lines
    in  (lineCount, rowDim)

loopProcess :: [Int] -> Handle -> IO [Int]
loopProcess xs handle = do
    eline <- (try $ L.hGetLine handle :: IO (Either IOError L.ByteString))
    case eline of
        Right line -> loopProcess (countCols line : xs) handle 
        Left _e    -> return xs

oneAtaTime :: FilePath -> IO [Int]
oneAtaTime filePath = do
    handle <- openFile filePath ReadMode
    withFile filePath ReadMode (loopProcess []) 

main = print =<< (oneAtaTime file) 
