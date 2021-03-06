module App.Codec
  ( decodeWord32s
  ) where

import Data.Word

import qualified Data.Binary.Get      as G
import qualified Data.ByteString.Lazy as LBS

decodeWord32s :: LBS.ByteString -> [Word32]
decodeWord32s = fmap (G.runGet G.getWord32le) . go
  where go :: LBS.ByteString -> [LBS.ByteString]
        go lbs = case LBS.splitAt 4 lbs of
          (lt, rt) -> if LBS.length lt == 4
            then lt:go rt
            else [LBS.take 4 (lt <> LBS.replicate 4 0) | LBS.length lt /= 0]
