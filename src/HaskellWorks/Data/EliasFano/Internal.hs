module HaskellWorks.Data.EliasFano.Internal
  ( lastMaybe
  ) where

lastMaybe :: [a] -> Maybe a
lastMaybe (a:as@(b:bs)) = lastMaybe as
lastMaybe [a]           = Just a
lastMaybe _             = Nothing
