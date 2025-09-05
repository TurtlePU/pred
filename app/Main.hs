{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Control.Applicative ((<**>))
import Options.Applicative qualified as Opt

import Pred.Prelude
import Pred.UI (runUI)

main :: IO ()
main = do
  filePath <- Opt.execParser $ Opt.info
    (Opt.strArgument
      (Opt.metavar "FILE" <> Opt.help "File to edit" <> Opt.action "file")
        <**> Opt.helper)
    (Opt.fullDesc <> Opt.progDesc ("PrEd is a Proof Editor, "
      <> "an IDE and a revelation. Thanks for coming to our TEDx"))
    -- | This doesn't really work btw but who cares
  runUI filePath
