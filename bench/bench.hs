{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main ( main) where

import           Criterion.Main (defaultMain, bcompare, bench, nf )
import           Control.DeepSeq ( NFData, rnf, deepseq )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import           Data.Word ( Word8 )

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData ByteString
#endif

main :: IO ()
main = do
  bs <- B.readFile "data/pg2189.txt"
  defaultMain
    [ bcompare
      [ bench "no-class"         $ nf    mk bs
      , bench "case-insensitive" $ nf CI.mk bs
      ]
    ]

--------------------------------------------------------------------------------

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
