module App.Commands where

import App.Commands.Demo
import App.Commands.Query
import Data.Semigroup      ((<>))
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdDemo
  <>  cmdQuery
