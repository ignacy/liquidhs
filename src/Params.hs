module Params (Params (..), cmdLineParser) where

import Data.Semigroup ((<>))
import Options.Applicative

data Params = Params {
                fname :: FilePath
              , parse :: Bool
              }

mkParams :: Parser Params
mkParams =
  Params <$>
             strArgument
             (metavar "FILE" <> help "Name of the Liquid file")
         <*> switch
             (long "parse" <> short 'p' <> help "only show parsign result")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (mkParams <**> helper)
                (fullDesc <> progDesc "Parse and render Liquid template")
