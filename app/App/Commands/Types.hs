{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( LoadSaveOptions(..)
  ) where

import GHC.Generics

data LoadSaveOptions = LoadSaveOptions
  { input  :: FilePath
  , output :: FilePath
  } deriving (Eq, Show, Generic)
