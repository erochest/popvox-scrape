{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main where


import           Control.Error
import           Control.Monad             ((<=<))
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Buildable
import           Data.Text.Format
import qualified Data.Text.Format          as F
import           Data.Text.Lazy.Builder    as B
import           Data.Traversable
import           Data.Version
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat)
import           Options.Applicative
import           Prelude                   hiding (FilePath, mapM)

import           Paths_popvox_scrape
import           PopVox.MapLight
import           PopVox.Output
import           PopVox.Types


sessions :: [Session]
sessions = [109, 110, 111, 112]


main :: IO ()
main = do
    PopVoxOptions{..} <- execParser opts

    F.print "Reading contributor data from {}..."
        . Only $ encodeString maplightDataDir
    ocIndex <-  fmap mconcat
            .   mapM (readIndexContribs `withLog` "\tReading input file {}...")
            =<< listDirectory maplightDataDir

    putStrLn "Querying maplight.org..."
    bIndex  <-  indexBills . concat . rights
            <$> mapM (billList maplightCacheDir mapLightUrl maplightApiKey
                        `withLog` "\tQuerying for session {}..."
                     ) sessions

    F.print "Writing data to {}..." $ Only outputFile
    writeOrgData (makeHeaderRow ocIndex bIndex)
                 (encodeString outputFile)
                 (toData bIndex ocIndex)

    putStrLn "done"


log' :: Buildable a => F.Format -> a -> IO a
log' f x = F.print f (Only x) >> return x

withLog :: Buildable a => (a -> IO b) -> F.Format -> a -> IO b
withLog m f a = F.print f (Only a) >> m a


instance Buildable FilePath where
    build = B.fromString . encodeString


opts' :: Parser PopVoxOptions
opts' =   PopVoxOptions
      <$> textOpt (  short 'k' <> long "api-key" <> metavar "API_KEY"
                  <> help "The API key to use when accessing the\
                          \ maplight.org API.")
      <*> fileOption (  short 'd' <> long "data-dir" <> value "./data"
                     <> metavar "MAPLIGHT_DATA_DIR"
                     <> help "The directory containing the contribution\
                             \ data. Defaults to './data'.")
      <*> fileOption (  short 'c' <> long "cache-dir"
                     <> metavar "MAPLIGHT_CACHE_DIR"
                     <> value "./.maplight-cache"
                     <> help "A directory to cache API responses in.\
                             \ Defaults to './.maplight-cache'.")
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
