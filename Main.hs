{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Main where


import           Control.Error
import           Control.Lens               hiding (argument)
import           Control.Monad              (forM_)
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Csv                   hiding (Only, Parser)
import           Data.Csv.Incremental       hiding (Parser, decodeByName)
import qualified Data.Csv.Incremental       as Csv
import qualified Data.HashMap.Strict        as M
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Format           hiding (print)
import qualified Data.Text.Format           as F
import           Data.Traversable
import qualified Data.Vector                as V
import           Data.Version
import           Filesystem
import           Filesystem.Path.CurrentOS  hiding (concat, decode)
import           Options.Applicative
import           Prelude                    hiding (FilePath, mapM)
import           System.IO                  (hFlush, stdout)

import           Paths_popvox_scrape
import           PopVox.Bills
import           PopVox.Contribs
import           PopVox.Legislators
import           PopVox.MapLight
import           PopVox.Output
import           PopVox.Ranks
import           PopVox.Types
import           PopVox.Utils


sessions :: [Session]
sessions = [109, 110, 111, 112, 113]


main :: IO ()
main = execParser opts >>= popvox

popvox :: PopVoxOptions -> IO ()
popvox Transform{..} = runScript $ do
    scriptIO $ putStrLn "\nQuerying maplight.org...\n"
    bIndex  <-  indexBills . concat
            <$> mapM (billList maplightAPIDir
                        `withLog` "\tQuerying for session {}...\n"
                     ) sessions
    scriptIO $ do
        dumpBillIndex bIndex
        F.print "\nReading contributor data from {}...\n"
            . Only $ encodeString contribDataFile
    ocIndex <- readIndexContribs contribDataFile
    scriptIO $ do
        dumpContribIndex ocIndex
        F.print "\nWriting data to {}...\n" $ Only outputFile
        writeOrgData (makeHeaderRow ocIndex bIndex)
                     outputFile
                     (toData bIndex ocIndex)

        putStrLn "\ndone\n"

popvox RankBills{..} = runScript $ do
    scriptIO $ putStrLn "rank-bills"

    pscores <-  fmap (indexPositionScores . concat)
            .   mapM readPositionScores
            =<< scriptIO (listDirectory rankBillScores)
    F.print "Read {} pscores.\n" . Only $ M.size pscores

    lindex  <-  fmap M.unions
            .   mapM readLegislatorIndex
            =<< scriptIO (listDirectory rankBillIndex)
    F.print "Read {} legislator IDs from '{}'.\n" (M.size lindex, rankBillIndex)

    bills   <-  map (resolveIDs lindex) <$> readBillDataDir rankBillBills
    F.print "Read {} bill sponsor information.\n" . Only $ length bills

    scriptIO . writeBillRanks rankBillOutput $ assembleRanks pscores bills

popvox TestJson{..} = forM_ sessions $ \s -> do
    F.print "\nQuerying for session {}... " $ Only s
    out <- runEitherT $ billList maplightAPIDir s
    case out of
        Right _ -> putStrLn "ok"
        Left e  -> F.print "ERROR: {}\n" $ Only e

popvox TestCsv{..} = do
    let fn = encodeString contribDataFile
    F.print "Reading {}... " $ Only fn
    hFlush stdout
    s <- map (<> "\n") . L8.lines <$> L8.readFile fn
    let header' = Csv.decodeByName :: HeaderParser (Csv.Parser OrgContrib)
        (headerOut, dataLines) = parseHeader header' s
    case headerOut of
        FailH dataLine er -> do
            F.print "HEADER ERROR: {}\n" $ Only er
            C8.putStrLn dataLine
        PartialH _ -> putStrLn "Oops! PartialH!"
        DoneH _ parser ->
            mapM_ print $ parseData parser dataLines
    putStrLn ""

    where
        parseHeader f@(FailH _ _) xs    = (f, xs)
        parseHeader (PartialH f) (x:xs) = parseHeader (f $ LB.toStrict x) xs
        parseHeader (PartialH f) []     = parseHeader (f B.empty) []
        parseHeader d@(DoneH _ _) xs    = (d, xs)

        parseData p = go p 2

        go (Fail _ er)   n _      =  [(n, er)]
        go (Done outs)   n _      =  leftLines n outs
        go (Many outs f) n []     =  leftLines n outs
                                  ++ go (f B.empty) (n + length outs) []
        go (Many outs f) n (x:xs) =
            let rest = go (f $ LB.toStrict x) (n + length outs) xs
            in  case leftLines n outs of
                    []  -> rest
                    xs' | failFast  -> xs'
                        | otherwise -> xs' ++ rest

        lft (Left x)  = Just x
        lft (Right _) = Nothing

        leftLines n = mapMaybe (sequenceA . fmap lft) . zip [n..]

popvox SearchPosition{..} = runScript $ do
    hits <-  fmap M.fromList . forM sessions $ \s ->
             (("session" <>) . T.pack . show $ getSession s,) . filter isHit
         <$> billList maplightAPIDir s
    scriptIO . LB.putStr $ Data.Aeson.encode hits
    where
        isHit :: BillInfo -> Bool
        isHit = any (maplightOrg `T.isInfixOf`) . getOrg
        getOrg bi = bi ^.. billInfoOrgs . traverse . orgInfoName

popvox ReportOn{..} = runScript $
        (EitherT . fmap decodeByName . LB.readFile . encodeString) reportCsvFile
    >>= V.mapM_ dumpOrg . V.filter isHit . snd
    where
        isHit = T.isInfixOf reportTarget . M.lookupDefault "" "Organization"
        dumpOrg :: M.HashMap B.ByteString T.Text -> Script ()
        dumpOrg org = scriptIO $ do
            mapM_ (F.print "\t{} => {}\n" . first Shown)
                . filter (not . T.null . snd)
                $ M.toList org
            putStrLn ""


transform' :: Parser PopVoxOptions
transform'
    =   Transform
    <$> fileOption (  short 'd' <> long "data-file" <> value "./data.csv"
                   <> metavar "CONTRIB_DATA_FILE"
                   <> help "The file containing the contribution\
                           \ data. Defaults to './data.csv'.")
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
    <$> fileOption (  short 'd' <> long "data-file" <> value "./data.csv"
                   <> metavar "CONTRIB_DATA_FILE"
                   <> help "The directory containing the contribution\
                           \ data. Defaults to './data.csv'.")
    <*> switch     (  short 'f' <> long "fail-fast"
                   <> help "Short-circuit processing on the first error.")

searchPosition' :: Parser PopVoxOptions
searchPosition'
    =   SearchPosition
    <$> fileOption (  short 'c' <> long "api-dir"
                   <> metavar "MAPLIGHT_API_DIR"
                   <> value "./maplight-cache"
                   <> help "A directory containing API responses.\
                           \ Defaults to './maplight-cache'.")
    <*> textArg    (  metavar "SEARCH_TARGET"
                   <> help "The name of the organization to search for.")

reportOn' :: Parser PopVoxOptions
reportOn'
    =   ReportOn
    <$> fileOption (  short 'c' <> long "csv-file"
                   <> metavar "CSV_OUTPUT_FILE"
                   <> value "./maplight-data.csv"
                   <> help "An output file to look for information\
                           \ about an organization.")
    <*> textArg    (  metavar "SEARCH_TARGET"
                   <> help "The name of the organization to search for.")

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
      <> command "search-pos"
         (info (helper <*> searchPosition')
            (progDesc "Search the bill information organizations."))
      <> command "report-on"
         (info (helper <*> reportOn')
            (progDesc "Report on the information in the data file for\
                      \ an organization."))
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

textArg :: Mod ArgumentFields T.Text -> Parser T.Text
textArg = argument (T.pack <$> str)
