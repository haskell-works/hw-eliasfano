{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( LoadSaveOptions(..)
  , CreateIndexOptions(..)
  ) where

import GHC.Generics

data LoadSaveOptions = LoadSaveOptions
  { input  :: FilePath
  , output :: FilePath
  } deriving (Eq, Show, Generic)

data CreateIndexOptions = CreateIndexOptions
  { input  :: FilePath
  , output :: FilePath
  } deriving (Eq, Show, Generic)
