module Main ( main ) where

import           Data.ByteString                    ( ByteString )
import qualified Data.ByteString.Char8      as BC8  ( pack, map )
import qualified Data.ByteString.Lazy       as Lazy ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BLC8 ( pack, map )
import qualified Data.CaseInsensitive       as CI   ( mk )
import           Data.Char                          ( toUpper, chr )
import           Data.Text                          ( Text )
import qualified Data.Text                  as T    ( pack, toUpper )
import qualified Data.Text.Lazy             as Lazy ( Text )
import qualified Data.Text.Lazy             as TL   ( pack, toUpper )
import           Test.Framework                     ( defaultMain, testGroup )
import           Test.Framework.Providers.HUnit     ( testCase )
import           Test.HUnit                         ( assertEqual )

main :: IO ()
main = defaultMain
  [ testGroup "ASCII"
    [ testCase "String"          $ assertEqual "" (CI.mk                    asciiStr)
                                                  (CI.mk (     map toUpper  asciiStr))
    , testCase "ByteString"      $ assertEqual "" (CI.mk                    asciiBs)
                                                  (CI.mk ( BC8.map toUpper  asciiBs))
    , testCase "Lazy.ByteString" $ assertEqual "" (CI.mk                    asciiLBs)
                                                  (CI.mk (BLC8.map toUpper  asciiLBs))
    , testCase "Text"            $ assertEqual "" (CI.mk                    asciiTxt)
                                                  (CI.mk (       T.toUpper  asciiTxt))
    , testCase "Lazy.Text"       $ assertEqual "" (CI.mk                    asciiLTxt)
                                                  (CI.mk (      TL.toUpper  asciiLTxt))
    ]
  , testGroup "ISO-8859-1"
    [ testCase "String"          $ assertEqual "" (CI.mk                    iso_8859_1Str)
                                                  (CI.mk (     map toUpper  iso_8859_1Str))
    , testCase "ByteString"      $ assertEqual "" (CI.mk                    iso_8859_1Bs)
                                                  (CI.mk ( BC8.map toUpper' iso_8859_1Bs))
    , testCase "Lazy.ByteString" $ assertEqual "" (CI.mk                    iso_8859_1LBs)
                                                  (CI.mk (BLC8.map toUpper' iso_8859_1LBs))
    , testCase "Text"            $ assertEqual "" (CI.mk                    iso_8859_1Txt)
                                                  (CI.mk (       T.toUpper  iso_8859_1Txt))
    , testCase "Lazy.Text"       $ assertEqual "" (CI.mk                    iso_8859_1LTxt)
                                                  (CI.mk (      TL.toUpper  iso_8859_1LTxt))
    ]
  ]


asciiLTxt :: Lazy.Text
asciiLTxt = TL.pack asciiStr

asciiTxt :: Text
asciiTxt = T.pack asciiStr

asciiLBs :: Lazy.ByteString
asciiLBs = BLC8.pack asciiStr

asciiBs :: ByteString
asciiBs = BC8.pack asciiStr

asciiStr :: String
asciiStr = map chr [0..127]


iso_8859_1LTxt :: Lazy.Text
iso_8859_1LTxt = TL.pack iso_8859_1Str

iso_8859_1Txt :: Text
iso_8859_1Txt = T.pack iso_8859_1Str

iso_8859_1LBs :: Lazy.ByteString
iso_8859_1LBs = BLC8.pack iso_8859_1Str

iso_8859_1Bs :: ByteString
iso_8859_1Bs = BC8.pack iso_8859_1Str

iso_8859_1Str :: String
iso_8859_1Str = asciiStr ++ map chr [128..255]


-- | Upper-casing some characters in ISO 8859-1 move them outside the 0-255 range.
toUpper' :: Char -> Char
toUpper' 'µ' = 'µ'       -- toUpper 'µ' (code point: 181) == 'Μ' (code point: 924)
toUpper' 'ÿ' = 'ÿ'       -- toUpper 'ÿ' (code point: 255) == 'Ÿ' (code point: 376)
toUpper' c   = toUpper c
