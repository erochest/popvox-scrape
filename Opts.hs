{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( opts
    , execParser
    ) where


import qualified Data.ByteString.Char8     as BS
import qualified Data.Text                 as T
import           Data.Version
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Options.Applicative.Types
import           Prelude                   hiding (FilePath)

import           Paths_popvox_scrape
import           PopVox.Types


opts' :: Parser PopVoxOptions
opts' =   PopVoxOptions
      <$> fileOpt (  short 's'
                  <> long "opensecrets"
                  <> metavar "DIRNAME"
                  <> help "The location of the opensecrets.org data sets.")
      <*> fileOpt (  short 'o'
                  <> long "output"
                  <> metavar "OUTPUT_FILE"
                  <> help "The CSV filename to write the data to.")
      <*> byteOpt (  short 'a'
                  <> long "api"
                  <> metavar "POPVOX_API"
                  <> help "You PopVox API key.")
      <*> switch  (  short 'v'
                  <> long "verbose"
                  <> help "Print extra information.")

opts :: ParserInfo PopVoxOptions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Scrapes data from popvox.com and \
                        \mashes it with opensecrets.org data."
            <> header ("popvox-scrape v" ++ showVersion version))

fileOpt :: Mod OptionFields FilePath -> Parser FilePath
fileOpt = option (decodeString <$> readerAsk)

textOpt :: Mod OptionFields T.Text -> Parser T.Text
textOpt = option (T.pack <$> readerAsk)

byteOpt :: Mod OptionFields BS.ByteString -> Parser BS.ByteString
byteOpt = option (BS.pack <$> readerAsk)
