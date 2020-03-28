{-# OPTIONS_GHC -fno-warn-orphans #-}
module OrphanInstances where

import           Data.String.Conversions (ConvertibleStrings (..), cs)


-- Instance allowing `cs` on lists and maybes of String, Text,
-- ByteString, etc.
instance (Functor f, ConvertibleStrings a b) => ConvertibleStrings (f a) (f b) where
  convertString = fmap cs
