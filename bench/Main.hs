{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Foreign                             hiding ((.&.))
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.EliasFano
import HaskellWorks.Data.FromForeignRegion
import System.Environment
import System.IO.MMap

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS

getPositions :: DVS.Vector Word64 -> [Word64]
getPositions v = DVS.ifoldl go id v []
  where go :: ([Word64] -> [Word64]) -> Int -> Word64 -> [Word64] -> [Word64]
        go ws i w = ws . goWord64 id 0 (fromIntegral i * 64) w

goWord64 :: ([Word64] -> [Word64]) -> Word64 -> Word64 -> Word64 -> [Word64] -> [Word64]
goWord64 ws i b w | i < 64  = if (w .&. 1) /= 0 then (b + i:) . goWord64 ws (i + 1) b (w .>. 1) else goWord64 ws (i + 1) b (w .>. 1)
goWord64 ws _ _ _ = ws

encode :: FilePath -> IO ()
encode filename = do
  !ibFr  <- mmapFileForeignPtr filename ReadOnly Nothing
  let !ib  = fromForeignRegion ibFr  :: DVS.Vector Word64
  let !positions = getPositions ib
  let !_ = fromWord64s positions :: EliasFano
  return ()

loadBitString :: FilePath -> IO BS.ByteString
loadBitString filepath = do
  (fptr :: ForeignPtr Word8, offset, sz) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset sz
  return bs

benchEliasFano :: [Benchmark]
benchEliasFano =
  [ bgroup "Load Elias Fano"
    [ bench "Load Elias Fano"  (whnfIO (encode "larger.ib"))
    ]
  ]

main :: IO ()
main = do
  args <- getArgs

  case args of
    (filename:_) -> encode filename
    _            -> defaultMain benchEliasFano

  putStrLn $ "Arguments: " <> show args
