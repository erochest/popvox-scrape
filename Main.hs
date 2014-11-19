{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Main where


import           Control.Error
import           Control.Monad             (forM_)
import qualified Data.ByteString.Lazy      as LB
import           Data.Csv                  hiding (Only, Parser)
import qualified Data.HashMap.Strict       as M
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Buildable
import           Data.Text.Format
import qualified Data.Text.Format          as F
import           Data.Text.Lazy.Builder    as B
import           Data.Traversable
import qualified Data.Vector               as V
import           Data.Version
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat, decode)
import           Options.Applicative
import           Prelude                   hiding (FilePath, mapM)
import           System.IO                 (hFlush, stdout)

import           Paths_popvox_scrape
import           PopVox.Bills
import           PopVox.Legislators
import           PopVox.MapLight
import           PopVox.Output
import           PopVox.Ranks
import           PopVox.Types


sessions :: [Session]
sessions = [109, 110, 111, 112]


main :: IO ()
main = execParser opts >>= popvox

popvox :: PopVoxOptions -> IO ()
popvox Transform{..} = runScript $ do
    scriptIO $ putStrLn "\nQuerying maplight.org...\n"
    bIndex  <-  indexBills . concat
            <$> mapM (billList maplightAPIDir
                        `withLog` "\tQuerying for session {}...\n"
                     ) sessions
    scriptIO $ dumpBillIndex bIndex

    scriptIO
        . F.print "\nReading contributor data from {}...\n"
        . Only $ encodeString maplightDataDir
    ocIndex <-  fmap (mconcat . map indexContribs')
            .   mapM (readContribs `withLog` "\tReading input file {}...\n")
            =<< scriptIO (listDirectory maplightDataDir)
    scriptIO $ dumpContribIndex ocIndex

    scriptIO . F.print "\nWriting data to {}...\n" $ Only outputFile
    scriptIO $ writeOrgData (makeHeaderRow ocIndex bIndex)
                 (encodeString outputFile)
                 (toData bIndex ocIndex)

    scriptIO $ putStrLn "\ndone\n"

popvox RankBills{..} = runScript $ do
    scriptIO $ putStrLn "rank-bills"

    pscores <-  fmap concat
            .   mapM readPositionScores
            =<< scriptIO (listDirectory rankBillScores)
    F.print "Read {} pscores.\n" . Only $ length pscores

    lindex  <-  fmap M.unions
            .   mapM readLegislatorIndex
            =<< scriptIO (listDirectory rankBillIndex)
    F.print "Read {} legislator IDs.\n" . Only $ M.size lindex

    bills   <-  readBillDataDir rankBillBills
    F.print "Read {} bill sponsor information.\n" . Only $ length bills

popvox TestJson{..} = forM_ sessions $ \s -> do
    F.print "\nQuerying for session {}... " $ Only s
    out <- runEitherT $ billList maplightAPIDir s
    case out of
        Right _ -> putStrLn "ok"
        Left e  -> F.print "ERROR: {}\n" $ Only e

popvox TestCsv{..} = do
    files <- map encodeString <$> listDirectory maplightDataDir
    forM_ files $ \fn -> do
        F.print "Reading {}... " $ Only fn
        hFlush stdout
        s <- LB.readFile fn
        let csv = snd <$> decodeByName s :: Either String (V.Vector OrgContrib)
        case csv of
            Right rows -> F.print "{} rows\n" . Only . V.length $ rows
            Left e     -> F.print "ERROR: {}\n" . Only $ Shown e

log' :: Buildable a => F.Format -> a -> IO a
log' f x = F.print f (Only x) >> return x

withLog :: Buildable a => (a -> Script b) -> F.Format -> a -> Script b
withLog m f a = scriptIO (F.print f $ Only a) >> m a

instance Buildable FilePath where
    build = B.fromString . encodeString

majorParty :: Party -> Bool
majorParty Dem = True
majorParty GOP = True
majorParty _   = False

indexContribs' :: (Header, V.Vector OrgContrib) -> OrgContribIndex
indexContribs' = indexContribs
               . V.filter (majorParty . contribParty . orgContribEntry)
               . snd


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

rankBills' :: Parser PopVoxOptions
rankBills'
    =   RankBills
    <$> fileOption (  short 's' <> long "score-dir"
                   <> metavar "VOTEVIEW_DATA_DIR"
                   <> help "The directory containing the voteview.com\
                           \ data directory.")
    <*> fileOption (  short 'b' <> long "bill-dir" <> metavar "BILL_DATA_DIR"
                   <> help "The directory containing the bill JSON data\
                           \ from govtrack.us.")
    <*> fileOption (  short 'l' <> long "legislator-index"
                   <> metavar "LEGISLATOR_INDEX_DIR"
                   <> help "The directory containing the legislator index\
                           \ data from github.com/unitedstates/congress-legislators.")
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
        (info (helper <*> transform')
            (progDesc "Read the data files, transform and join, and output."))
      <> command "rank-bills"
         (info (helper <*> rankBills')
            (progDesc "Determine the bias of each bill from the sponsor's scores."))
      <> command "test-json"
         (info (helper <*> testJson')
            (progDesc "Read the JSON API files and test that we can parse them."))
      <> command "test-csv"
         (info (helper <*> testCsv')
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
