{-# LANGUAGE OverloadedStrings #-}


module Main where


import qualified Data.Text as T
import           Data.Version
import           Options.Applicative

import           Paths_popvox_scrape
import           PopVox.Types


main :: IO ()
main = print =<< execParser opts


opts' :: Parser PopVoxOptions
opts' =   PopVoxOptions
      <$> textOpt (  short 'k' <> long "api-key" <> metavar "API_KEY"
                  <> help "The API key to use when accessing the\
                          \ maplight.org API.")
      <*> strOption (  short 'd' <> long "data-dir" <> value "./data"
                    <> metavar "MAPLIGHT_DATA_DIR"
                    <> help "The directory containing the contribution\
                            \ data. Defaults to './data'.")
      <*> strOption (  short 'o' <> long "output" <> metavar "CSV_OUTPUT"
                    <> help "The file to write the output to.")

opts :: ParserInfo PopVoxOptions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Assemble a dataset of bill positions and\
                        \ political contributions."
            <> header ("popvox-scrape v" ++ showVersion version))

textOpt :: Mod OptionFields T.Text -> Parser T.Text
textOpt = option (T.pack <$> str)
