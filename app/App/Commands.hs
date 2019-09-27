module App.Commands where

import App.Commands.Count
import App.Commands.CreateBlankedXml
import App.Commands.CreateBpIndex
import App.Commands.CreateIbIndex
import App.Commands.CreateIndex
import App.Commands.Demo
import Data.Semigroup                ((<>))
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdCount
  <>  cmdCreateIndex
  <>  cmdCreateBlankedXml
  <>  cmdCreateIbIndex
  <>  cmdCreateBpIndex
  <>  cmdDemo
