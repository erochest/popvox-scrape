{-# LANGUAGE OverloadedStrings #-}


module Opts
    ( opts
    , execParser
    ) where


import qualified Data.Text                 as T
import           Data.Version
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                   hiding (FilePath)

import           Paths_popvox_scrape
import           PopVox.Types


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

extractCF' :: Parser PopVoxOptions
extractCF'
    =   ExtractCF
    <$> fileOption (  short 'd' <> long "data-file" <> value "./data.csv"
                   <> metavar "CONTRIB_DATA_FILE"
                   <> help "The file containing the contribution\
                           \ data. Defaults to './data.csv'.")
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
      <> command "extract-cf"
         (info (helper <*> extractCF')
            (progDesc "Extract the CF scores from the contributor data file."))
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

fileOption :: Mod OptionFields FilePath -> Parser FilePath
fileOption = option (decodeString <$> str)

textArg :: Mod ArgumentFields T.Text -> Parser T.Text
textArg = argument (T.pack <$> str)
