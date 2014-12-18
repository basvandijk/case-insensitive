{-# LANGUAGE CPP, NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CaseInsensitive.Unsafe
-- Copyright   :  (c) 2011-2013 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Provides an unsafe way to create a case insensitive string-like value.
--
-----------------------------------------------------------------------------

module Data.CaseInsensitive.Unsafe   ( unsafeMk ) where
import Data.CaseInsensitive.Internal ( unsafeMk )
