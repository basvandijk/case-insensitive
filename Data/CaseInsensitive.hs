{-# LANGUAGE CPP
           , NoImplicitPrelude
           , UnicodeSyntax
           , TypeSynonymInstances
           , DeriveDataTypeable
           , FlexibleInstances
  #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CaseInsensitive
-- Copyright   :  (c) 2011-2012 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module is intended to be imported qualified. May I suggest:
--
-- @
-- import           Data.CaseInsensitive  ( CI )
-- import qualified Data.CaseInsensitive as CI
-- @
--
-----------------------------------------------------------------------------

module Data.CaseInsensitive ( CI
                            , mk
                            , original
                            , foldedCase
                            , map
                            , FoldCase(foldCase)
                            ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Eq       ( Eq, (==) )
import Data.Ord      ( Ord, compare )
import Data.Function ( on )
import Data.Monoid   ( Monoid, mempty, mappend )
import Data.String   ( IsString, fromString )
import Data.Typeable ( Typeable )
import Data.Word     ( Word8 )
import Prelude       ( String, (.), fmap, (&&), (+), (<=), (>=), otherwise )
import Text.Read     ( Read, readPrec )
import Text.Show     ( Show, showsPrec, ShowS )

-- from bytestring:
import qualified Data.ByteString             as B    ( ByteString, map )
import qualified Data.ByteString.Lazy        as BL   ( ByteString, map )

-- from text:
import qualified Data.Text      as T  ( Text, toCaseFold )
import qualified Data.Text.Lazy as TL ( Text, toCaseFold, pack, unpack )

-- from hashable:
import Data.Hashable ( Hashable(hash) )

--------------------------------------------------------------------------------
-- Case Insensitive Strings
--------------------------------------------------------------------------------

{-| A @CI s@ provides /C/ase /I/nsensitive comparison for the string-like type
@s@ (for example: 'String', 'T.Text', 'B.ByteString', 'ShowS', etc.).

Note that @CI s@ has an instance for 'IsString' which together with the
@OverloadedStrings@ language extension allows you to write case insensitive
string literals as in:

@
\> (\"Content-Type\" :: 'CI' 'T.Text') == (\"CONTENT-TYPE\" :: 'CI' 'T.Text')
True
@

-}
data CI s = CI { original   ∷ !s -- ^ Retrieve the original string-like value.
               , foldedCase ∷ !s -- ^ Retrieve the case folded string-like value.
                                 --   (Also see 'foldCase').
               }
          deriving Typeable

-- | Make the given string-like value case insensitive.
mk ∷ FoldCase s ⇒ s → CI s
mk s = CI s (foldCase s)

-- | Transform the original string-like value but keep it case insensitive.
map ∷ FoldCase s2 ⇒ (s1 → s2) → (CI s1 → CI s2)
map f = mk . f . original

instance (IsString s, FoldCase s) ⇒ IsString (CI s) where
    fromString = mk . fromString

instance Monoid s ⇒ Monoid (CI s) where
    mempty = CI mempty mempty
    CI o1 l1 `mappend` CI o2 l2 = CI (o1 `mappend` o2) (l1 `mappend` l2)

instance Eq s ⇒ Eq (CI s) where
    (==) = (==) `on` foldedCase

instance Ord s ⇒ Ord (CI s) where
    compare = compare `on` foldedCase

instance (Read s, FoldCase s) ⇒ Read (CI s) where
    readPrec = fmap mk readPrec

instance Show s ⇒ Show (CI s) where
    showsPrec prec = showsPrec prec . original

instance Hashable s => Hashable (CI s) where
  hash = hash . foldedCase


--------------------------------------------------------------------------------
-- Folding (lowering) cases
--------------------------------------------------------------------------------

-- | Class of string-like types that support folding cases.
--
-- /Note/: In some languages, case conversion is a locale- and context-dependent
-- operation. The @foldCase@ method is /not/ intended to be locale sensitive.
-- Programs that require locale sensitivity should use appropriate versions of
-- the case mapping functions from the @text-icu@ package:
-- <http://hackage.haskell.org/package/text-icu>
class FoldCase s where foldCase ∷ s → s

-- | Note that @foldCase = 'B.map' toLower@ which is only guaranteed to be correct for ASCII encoded strings!
instance FoldCase B.ByteString where foldCase = B.map toLower

-- | Note that @foldCase = 'BL.map' toLower@ which is only guaranteed to be correct for ASCII encoded strings!
instance FoldCase BL.ByteString where foldCase = BL.map toLower

instance FoldCase String  where foldCase = TL.unpack . TL.toCaseFold . TL.pack
instance FoldCase T.Text  where foldCase = T.toCaseFold
instance FoldCase TL.Text where foldCase = TL.toCaseFold
instance FoldCase ShowS   where foldCase = (foldCase .)
instance FoldCase (CI s)  where foldCase (CI _ l) = CI l l

toLower :: Word8 -> Word8
toLower w
  | 65 <= w && w <= 90 = w + 32
  | otherwise          = w
