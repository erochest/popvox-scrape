{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main where


import           Control.Error
import           Control.Exception
import           Control.Monad                (forM_)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy         as LB
import           Data.Conduit
import           Data.Conduit.List            (consume)
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion  (Named(..))
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Buildable
import           Data.Text.Format
import qualified Data.Text.Format             as F
import           Data.Text.Lazy.Builder       as B
import           Data.Traversable
import qualified Data.Vector                  as V
import           Data.Version
import           Filesystem
import           Filesystem.Path.CurrentOS    hiding (concat)
import           Options.Applicative
import           Prelude                      hiding (FilePath, mapM)
import           System.IO                    (hFlush, stdout)

import           Paths_popvox_scrape
import           PopVox.MapLight
import           PopVox.Output
import           PopVox.Types


sessions :: [Session]
sessions = [109, 110, 111, 112]


main :: IO ()
main = execParser opts >>= popvox

popvox :: PopVoxOptions -> IO ()
popvox Transform{..} = do
    putStrLn "\nQuerying maplight.org...\n"
    bIndex  <-  indexBills . concat . rights
            <$> mapM (billList maplightAPIDir
                        `withLog` "\tQuerying for session {}...\n"
                     ) sessions
    dumpBillIndex bIndex

    F.print "\nReading contributor data from {}...\n"
        . Only $ encodeString maplightDataDir
    ocIndex <-  fmap mconcat
            .   mapM (readIndexContribs `withLog` "\tReading input file {}...\n")
            =<< listDirectory maplightDataDir
    dumpContribIndex ocIndex

    F.print "\nWriting data to {}...\n" $ Only outputFile
    writeOrgData (makeHeaderRow ocIndex bIndex)
                 (encodeString outputFile)
                 (toData bIndex ocIndex)

    putStrLn "\ndone\n"

popvox TestJson{..} = forM_ sessions $ \s -> do
    F.print "\nQuerying for session {}... " $ Only s
    out <- billList maplightAPIDir s
    case out of
        Right _ -> putStrLn "ok"
        Left e  -> F.print "ERROR: {}\n" $ Only e

popvox TestCsv{..} = do
    files <- map encodeString <$> listDirectory maplightDataDir
    forM_ files $ \fn -> do
        F.print "Reading {}... " $ Only fn
        hFlush stdout
        s <- LB.readFile fn
        let csv = decodeCSV defCSVSettings s :: Either SomeException (V.Vector (Named OrgContrib))
        case csv of
            Right rows ->
                return ()
                -- F.print "{} rows\n" . Only . length $ rows
            Left err -> F.print "ERROR: {}\n" . Only $ Shown err

log' :: Buildable a => F.Format -> a -> IO a
log' f x = F.print f (Only x) >> return x

withLog :: Buildable a => (a -> IO b) -> F.Format -> a -> IO b
withLog m f a = F.print f (Only a) >> m a


instance Buildable FilePath where
    build = B.fromString . encodeString


transform' :: Parser PopVoxOptions
transform'
    =   Transform
    <$> fileOption (  short 'd' <> long "data-dir" <> value "./data"
                   <> metavar "MAPLIGHT_DATA_DIR"
                   <> help "The directory containing the contribution\
                           \ data. Defaults to './data'.")
    <*> fileOption (  short 'c' <> long "api-dir"
                   <> metavar "MAPLIGHT_API_DIR"
                   <> value "./maplight-cache"
                   <> help "A directory containing API responses.\
                           \ Defaults to './maplight-cache'.")
    <*> fileOption (  short 'o' <> long "output" <> metavar "CSV_OUTPUT"
                   <> help "The file to write the output to.")

testJson' :: Parser PopVoxOptions
testJson'
    =   TestJson
    <$> fileOption (  short 'c' <> long "api-dir"
                   <> metavar "MAPLIGHT_API_DIR"
                   <> value "./maplight-cache"
                   <> help "A directory containing API responses.\
                           \ Defaults to './maplight-cache'.")

testCsv' :: Parser PopVoxOptions
testCsv'
    =   TestCsv
    <$> fileOption (  short 'd' <> long "data-dir" <> value "./data"
                   <> metavar "MAPLIGHT_DATA_DIR"
                   <> help "The directory containing the contribution\
                           \ data. Defaults to './data'.")

opts' :: Parser PopVoxOptions
opts' = subparser
      ( command "transform"
        (info transform'
            (progDesc "Read the data files, transform and join, and output."))
      <> command "test-json"
         (info testJson'
            (progDesc "Read the JSON API files and test that we can parse them."))
      <> command "test-csv"
         (info testCsv'
            (progDesc "Read the CSV data files and test that we can parse them."))
      )

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
