{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.LoadSave
  ( cmdLoadSave
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))
import Data.Word
import Options.Applicative       hiding (columns)

import qualified App.Commands.Types                            as Z
import qualified Data.Binary.Get                               as G
import qualified Data.ByteString.Lazy                          as LBS
import qualified Data.Vector.Storable                          as DVS
import qualified HaskellWorks.Data.EliasFano                   as EF
import qualified HaskellWorks.Data.PackedVector.PackedVector64 as PV
import qualified System.IO                                     as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

decodeWord32s :: LBS.ByteString -> [Word32]
decodeWord32s = fmap (G.runGet G.getWord32le) . go
  where go :: LBS.ByteString -> [LBS.ByteString]
        go lbs = case LBS.splitAt 4 lbs of
          (lt, rt) -> if LBS.length lt == 4
            then lt:go rt
            else if LBS.length lt == 0
              then []
              else [LBS.take 4 (lt <> LBS.replicate 4 0)]

runLoadSave :: Z.LoadSaveOptions -> IO ()
runLoadSave opts = do
  lbs <- LBS.readFile (opts ^. the @"input")
  let ws = fmap fromIntegral (decodeWord32s lbs) :: [Word64]
  let ef = EF.fromListWord64 ws :: EF.EliasFano
  -- let x = DVS.length (PV.swBuffer (EF.efCount ef))
  IO.putStrLn $ "Eliasfano:"
    <> " bits: "  <> show (ef & EF.efBucketBits & DVS.length                )
    <> " count: " <> show (ef & EF.efLoSegments & PV.swBuffer & DVS.length  )
    <> " pv: "    <> show (ef & EF.efCount                                  )
    <> " lbc: "   <> show (ef & EF.efLoBitCount                             )

optsLoadSave :: Parser Z.LoadSaveOptions
optsLoadSave = Z.LoadSaveOptions
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

cmdLoadSave :: Mod CommandFields (IO ())
cmdLoadSave = command "load-save"  $ flip info idm $ runLoadSave <$> optsLoadSave
