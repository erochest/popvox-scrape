{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Error
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Traversable
import           Data.Version
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat)
import           Options.Applicative
import           Prelude                   hiding (FilePath, mapM)

import           Paths_popvox_scrape
import           PopVox.MapLight
import           PopVox.Types


sessions :: [Session]
sessions = [109, 110, 111, 112]


-- TODO: Need to progress messages....
main :: IO ()
main = do
    pv@PopVoxOptions{..} <- execParser opts
    ocIndex <-  fmap mconcat
            .   mapM readIndexContribs
            =<< listDirectory maplightDataDir
    bIndex  <-  indexBills . concat . rights
            <$> mapM (billList mapLightUrl maplightApiKey) sessions
    let orgs = toData bIndex ocIndex
    undefined


opts' :: Parser PopVoxOptions
opts' =   PopVoxOptions
      <$> textOpt (  short 'k' <> long "api-key" <> metavar "API_KEY"
                  <> help "The API key to use when accessing the\
                          \ maplight.org API.")
      <*> fileOption (  short 'd' <> long "data-dir" <> value "./data"
                     <> metavar "MAPLIGHT_DATA_DIR"
                     <> help "The directory containing the contribution\
                             \ data. Defaults to './data'.")
      <*> fileOption (  short 'o' <> long "output" <> metavar "CSV_OUTPUT"
                     <> help "The file to write the output to.")

opts :: ParserInfo PopVoxOptions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Assemble a dataset of bill positions and\
                        \ political contributions."
            <> header ("popvox-scrape v" ++ showVersion version))

textOpt :: Mod OptionFields T.Text -> Parser T.Text
textOpt = option (T.pack <$> str)

fileOption :: Mod OptionFields FilePath -> Parser FilePath
fileOption = option (decodeString <$> str)
