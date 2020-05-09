{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Codec
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Log2
import HaskellWorks.Data.PackedVector.Internal
import HaskellWorks.Data.Positioning
import Options.Applicative                     hiding (columns)

import qualified App.Commands.Types                   as Z
import qualified Data.ByteString.Builder              as B
import qualified Data.ByteString.Lazy                 as LBS
import qualified HaskellWorks.Data.EliasFano.Internal as EF
import qualified System.Exit                          as IO
import qualified System.IO                            as IO
import qualified System.IO.Temp                       as RIO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

readWord32s :: MonadIO m => FilePath -> m [Word32]
readWord32s fp = do
  lbs <- liftIO $ LBS.readFile fp

  return (decodeWord32s lbs)

w32To64 :: Word32 -> Word64
w32To64 = fromIntegral

runCreateIndex :: Z.CreateIndexOptions -> IO ()
runCreateIndex opts = runResourceT $ do
  let output = opts ^. the @"output"
  (maybeEnd, count) <- EF.foldCountAndLast <$> readWord32s (opts ^. the @"input")

  case maybeEnd of
    Just end -> do
      let length'   = count
      let loBits'   = fromIntegral (log2 (fromIntegral end `EF.divup` length')) :: Count
      let hiMask    = maxBound .<. loBits' :: Word64
      let loMask    = comp hiMask :: Word64
      his <- fmap ((.>. loBits') . (.&. hiMask) . w32To64) <$> readWord32s (opts ^. the @"input")
      los <- fmap ((.&. loMask)                 . w32To64) <$> readWord32s (opts ^. the @"input")

      let hiWords       = EF.hiSegmentToWords his
      let loWords       = packBits loBits' los
      let efLoBitCount  = loBits'
      let efCount       = length'

      liftIO . IO.putStrLn $ "Lo bit count: " <> show efLoBitCount
      liftIO . IO.putStrLn $ "Entries: " <> show efCount

      (_, _, hHi) <- RIO.openBinaryTempFile Nothing (output <> ".hi")
      (_, _, hLo) <- RIO.openBinaryTempFile Nothing (output <> ".lo")

      liftIO $ LBS.hPut hHi (B.toLazyByteString (foldMap B.word64LE hiWords)) >> IO.hFlush hHi
      liftIO $ LBS.hPut hLo (B.toLazyByteString (foldMap B.word64LE loWords)) >> IO.hFlush hLo

      liftIO $ IO.hSeek hHi IO.AbsoluteSeek 0
      liftIO $ IO.hSeek hLo IO.AbsoluteSeek 0

      hiSize <- liftIO $ IO.hFileSize hHi
      loSize <- liftIO $ IO.hFileSize hLo

      loBytes <- liftIO $ LBS.hGetContents hLo
      hiBytes <- liftIO $ LBS.hGetContents hHi

      liftIO $ LBS.writeFile output $ B.toLazyByteString $ mempty
        <> B.word64LE count
        <> B.word64LE (fromIntegral loSize)
        <> B.word64LE (fromIntegral hiSize)
        <> B.lazyByteString loBytes
        <> B.lazyByteString hiBytes

      return ()

    Nothing -> do
      liftIO $ IO.hPutStrLn IO.stderr "Empty input"
      liftIO   IO.exitFailure

optsCreateIndex :: Parser Z.CreateIndexOptions
optsCreateIndex = Z.CreateIndexOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input file of little-endian monotonically increasing word32s"
        <>  metavar "FILE"
        )
  <*> strOption
        (   long "output"
        <>  short 'o'
        <>  help "Output files"
        <>  metavar "FILE"
        )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
