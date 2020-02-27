import qualified Data.ByteString.Char8 as L 
import Data.List
import System.IO
import Control.Exception
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Control.Monad.Trans.Resource

file :: ConduitM () L.ByteString ResIO ()
file = C.sourceFile "char.csv"

newChar char = toEnum . fromEnum $ char

countCols = length . L.split ','

countLines :: ConduitM () Void ResIO Integer
countLines = file .| C.splitOnUnboundedE (== newChar '\n') .| C.length 

getLines :: ConduitM () L.ByteString ResIO ()
getLines = file .| C.splitOnUnboundedE (== newChar '\n')   

getLengths :: ConduitM () Int ResIO ()
getLengths = getLines .| C.map countCols  

--main = nub <$> (runResourceT  $ runConduit getLengths)
    

