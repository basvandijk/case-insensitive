module NoClass ( CI, mk ) where

import           Control.DeepSeq            ( NFData, rnf, deepseq )
import           Data.ByteString            ( ByteString )
import qualified Data.ByteString      as B  ( map )
import           Data.Word                  ( Word8 )

data CI s = CI !s !s

instance NFData s => NFData (CI s) where
    rnf (CI o f) = o `deepseq` f `deepseq` ()

mk :: ByteString -> CI ByteString
mk s = CI s (foldCase s)

foldCase :: ByteString -> ByteString
foldCase = B.map toLower8

toLower8 :: Word8 -> Word8
toLower8 w
  |  65 <= w && w <=  90 ||
    192 <= w && w <= 214 ||
    216 <= w && w <= 222 = w + 32
  | otherwise            = w
