module App.Commands where

import App.Commands.CreateIndex
import App.Commands.LoadSave
import Options.Applicative

{- HLINT ignore "Monoid law, left identity"  -}

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdLoadSave
  <>  cmdCreateIndex
